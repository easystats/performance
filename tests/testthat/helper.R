# Variant for plot snapshots that differ between operating systems. Linux
# snapshots live in `_snaps/linux/`, all others in `_snaps/windows`. Calling one
# `expect_doppelganger()` per plot with this variant keeps the snapshot names
# the same on every OS, so a local test run does not delete the snapshots of
# the other OS.
snapshot_variant <- function() {
  switch(Sys.info()[["sysname"]], Linux = "linux", Windows = "windows", "mac")
}
