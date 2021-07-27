{ pkgs ? import <nixpkgs> {}, ... }:

let
  src = pkgs.writeText "main.c" ''
#define _GNU_SOURCE
#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <signal.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <sys/mount.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>

#define err_exit(format, ...) { fprintf(stderr, format ": %s\n", ##__VA_ARGS__, strerror(errno)); exit(EXIT_FAILURE); }

static int pivot_root(const char *new_root, const char *put_old) {
   return syscall(SYS_pivot_root,new_root,put_old);
}

static void usage(char *pname) {
    fprintf(stderr, "Usage: %s <gnupath> <command>\n", pname);

    exit(EXIT_FAILURE);
}

static void update_map(char *mapping, char *map_file) {
    int fd;

    fd = open(map_file, O_WRONLY);
    if (fd < 0) {
        err_exit("map open");
    }

    int map_len = strlen(mapping);
    if (write(fd, mapping, map_len) != map_len) {
        err_exit("map write");
    }

    close(fd);
}

int main(int argc, char *argv[]) {
    char map_buf[1024];
    char path_buf[PATH_MAX];
    char path_buf2[PATH_MAX];
    char cwd[PATH_MAX];

    if (argc < 3) {
        usage(argv[0]);
    }

    char *tmpdir = getenv("TMPDIR");
    if (!tmpdir) {
        tmpdir = "/tmp";
    }

    char template[PATH_MAX];
    int needed = snprintf(template, PATH_MAX, "%s/.guix.XXXXXX", tmpdir);
    if (needed < 0) {
        err_exit("TMPDIR too long: '%s'", tmpdir);
    }

    char *rootdir = mkdtemp(template);
    if (!rootdir) {
        err_exit("mkdtemp(%s)", template);
    }

    char *gnudir = realpath(argv[1], NULL);
    if (!gnudir) {
        err_exit("realpath(%s)", argv[1]);
    }

    // save cwd to restore it later
    if (!getcwd(cwd, PATH_MAX)) {
        err_exit("getcwd()");
    }

    uid_t uid = getuid();
    gid_t gid = getgid();

    if (unshare(CLONE_NEWNS | CLONE_NEWUSER) < 0) {
        err_exit("unshare()");
    }

    // prepare pivot_root call:
    // rootdir must be a mount point
    if (mount(rootdir, rootdir, "none", MS_BIND | MS_REC, NULL) < 0) {
        err_exit("mount --bind %s %s", rootdir, rootdir);
    }
    if (mount(rootdir, rootdir, "none", MS_PRIVATE | MS_REC, NULL) < 0) {
        err_exit("mount --make-rprivate %s", rootdir);
    }

    // create the mount point for the old root
    // The old root cannot be unmounted/removed after pivot_root, the only way to
    // keep / clean is to hide the directory with another mountpoint. Therefore
    // we pivot the old root to /gnu. This is somewhat confusing, though.
    snprintf(path_buf, sizeof(path_buf), "%s/gnu", rootdir);
    if (mkdir(path_buf, 0) < 0) { // the mode is irrelevant
        err_exit("mkdir(%s, 0)", path_buf);
    }

    // pivot_root
    if (pivot_root(rootdir, path_buf) < 0) {
        err_exit("pivot_root(%s, %s)", rootdir, path_buf);
    }
    chdir("/");

    // bind mount all / stuff into rootdir
    // the orginal content of / now available under /gnu
    DIR* d = opendir("/gnu");
    if (!d) {
        err_exit("open /gnu");
    }

    struct dirent *ent;
    while ((ent = readdir(d))) {
        // do not bind mount an existing guix installation
        if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, "..") || !strcmp(ent->d_name, "gnu")) {
            continue;
        }

        snprintf(path_buf, sizeof(path_buf), "/gnu/%s", ent->d_name);

        struct stat statbuf;
        if (stat(path_buf, &statbuf) < 0) {
            fprintf(stderr, "Cannot stat %s: %s\n", path_buf, strerror(errno));
            continue;
        }

        snprintf(path_buf2, sizeof(path_buf2), "/%s", ent->d_name);

        if (S_ISDIR(statbuf.st_mode)) {
            mkdir(path_buf2, statbuf.st_mode & ~S_IFMT);
            if (mount(path_buf, path_buf2, "none", MS_BIND | MS_REC, NULL) < 0) {
                fprintf(stderr, "Cannot bind mount %s to %s: %s\n", path_buf, path_buf2, strerror(errno));
            }
        }
    }

    // mount the store and hide the old root
    // we fetch gnudir under the old root
    snprintf(path_buf, sizeof(path_buf), "/gnu/%s", gnudir);
    if (mount(path_buf, "/gnu", "none", MS_BIND | MS_REC, NULL) < 0) {
        err_exit("mount --bind %s /gnu", path_buf);
    }

    // fixes issue #1 where writing to /proc/self/gid_map fails
    // see user_namespaces(7) for more documentation
    int fd_setgroups = open("/proc/self/setgroups", O_WRONLY);
    if (fd_setgroups > 0) {
        write(fd_setgroups, "deny", 4);
    }

    // map the original uid/gid in the new ns
#ifdef FAKEROOT
    snprintf(map_buf, sizeof(map_buf), "%d %d 1", 0, uid);
#else
    snprintf(map_buf, sizeof(map_buf), "%d %d 1", uid, uid);
#endif
    update_map(map_buf, "/proc/self/uid_map");

#ifdef FAKEROOT
    snprintf(map_buf, sizeof(map_buf), "%d %d 1", 0, gid);
#else
    snprintf(map_buf, sizeof(map_buf), "%d %d 1", gid, gid);
#endif
    update_map(map_buf, "/proc/self/gid_map");

    // restore cwd
    chdir(cwd);

    // execute the command
    setenv("GUIX_CONFIGURATION_DIRECTORY", "/gnu/etc/guix", 1);
    setenv("GUIX_STATE_DIRECTORY", "/gnu/var", 1);
    setenv("GUIX_LOG_DIRECTORY", "/gnu/var/log", 1);
    setenv("GUIX_DATABASE_DIRECTORY", "/gnu/var/db", 1);
    execvp(argv[2], argv+2);
    err_exit("execvp(%s)", argv[2]);
}
  '';
in pkgs.stdenv.mkDerivation rec {
  name = "guix-ns";
  inherit src;
  dontUnpack = true;
  buildPhase = "$CC $src -o user; $CC -DFAKEROOT $src -o root";
  installPhase = ''mkdir -p $out/bin;
    mv user $out/bin/${name}-user
    mv root $out/bin/${name}-root
    cd $out/bin && ln -s ${name}-root ${name}
  '';
}

## Local Variables: ***
## mode: nix-dsquoted-c ***
## End: ***
