{ ... }:

{
    programs.mpv.enable = true;
    programs.mpv.config = {
      hwdec = "auto-safe";
      vo = "gpu";
      profile = "gpu-hq";
    };
}
