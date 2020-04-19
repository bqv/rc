final: prev:

{
  qemu-user-arm = if final.stdenv.system == "x86_64-linux"
    then final.pkgsi686Linux.callPackage ./qemu { user_arch = "arm"; }
    else final.callPackage      ./qemu { user_arch = "arm"; };
  qemu-user-x86 = final.callPackage ./qemu { user_arch = "x86_64"; };
  qemu-user-arm64 = final.callPackage ./qemu { user_arch = "aarch64"; };
  qemu-user-riscv32 = final.callPackage ./qemu { user_arch = "riscv32"; };
  qemu-user-riscv64 = final.callPackage ./qemu { user_arch = "riscv64"; };
}
