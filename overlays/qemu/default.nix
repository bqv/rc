self: super:

{
  qemu-user-arm = if self.stdenv.system == "x86_64-linux"
    then self.pkgsi686Linux.callPackage ./qemu { user_arch = "arm"; }
    else self.callPackage      ./qemu { user_arch = "arm"; };
  qemu-user-x86 = self.callPackage ./qemu { user_arch = "x86_64"; };
  qemu-user-arm64 = self.callPackage ./qemu { user_arch = "aarch64"; };
  qemu-user-riscv32 = self.callPackage ./qemu { user_arch = "riscv32"; };
  qemu-user-riscv64 = self.callPackage ./qemu { user_arch = "riscv64"; };
}
