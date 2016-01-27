{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.services.logster;

  nginxLogster = pkgs.fetchgit {
    url = "git://github.com:metabrainz/logster.git";
    rev = "6815946c0e912222b20a5807ea9fee717c4a65ab";
    sha256 = "0i5l672xbmaflybl99svnwrklb8fhxsba5fgln6q0n3lyxvxzbi0";
  };

  accessLogOption = mkOptionType {
    name = "nginx access log file";
    inherit (types.str) check merge;
  };

in {
  options = {
    services.logster = {
      enable = mkOption {
        type = types.bool;
      };

      accessLog = mkOption {
        type = types.attrsOf accessLogOption;
      };

      graphiteHost = mkOption {
        type = types.string;
      };

      prefix = mkOption {
        type = types.string;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.logster = {
      environment = {
        PYTHONPATH = nginxLogster;
      };
      wantedBy = [ "multi-user.target" ];
      serviceConfig.ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /var/lib/logster";
      script =
        let runOne = name: logFile:
            ''
              ${pkgs.pythonPackages.logster}/bin/logster \
                --metric-prefix ${cfg.prefix}.${name} \
                --graphite-host ${cfg.graphiteHost} \
                --state-dir /var/lib/logster \
                --output graphite \
                --logtail ${pkgs.logcheck}/sbin/logtail2 \
                musicbrainz.logster.NginxStatus.NginxStatus \
                ${logFile}
            '';
        in concatStrings (mapAttrsToList runOne cfg.accessLog);
    };

    systemd.timers.logster = {
      timerConfig.OnUnitActiveSec = "1m";
      timerConfig.OnBootSec = "0s";
    };
  };
}