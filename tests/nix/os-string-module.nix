{
  # Needed since GHC 9.10
  packages.file-io.flags.os-string = true;
  packages.filepath.flags.os-string = true;
  packages.directory.flags.os-string = true;
  packages.process.flags.os-string = true;
  packages.unix.flags.os-string = true;

  packages.Win32.flags.os-string = true;
}
