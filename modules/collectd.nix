{ config, pkgs, ... }:
with pkgs.lib;
let
  cfg = config.services.collectd;

in {
  options = {
    services.collectd = {
      prefix = mkOption {
        type = types.string;
        description = "A common prefix to add to all collectd stats, excluding trailing '.'";
      };
    };
  };

  config = {
    systemd.services.collectd = {
      enable = true;
      serviceConfig = {
        ExecStart =
          let
          collectdConf = pkgs.writeText "collectd.conf"
            ''
            Interval 10

            LoadPlugin Exec
            LoadPlugin cpu
            LoadPlugin df
            LoadPlugin disk
            LoadPlugin interface
            LoadPlugin load
            LoadPlugin memory
            LoadPlugin network
            LoadPlugin postgresql
            LoadPlugin processes
            LoadPlugin redis
            LoadPlugin syslog
            LoadPlugin tcpconns
            LoadPlugin write_graphite

            <Plugin tcpconns>
              ListeningPorts true
            </Plugin>

            <Plugin write_graphite>
              <Carbon>
                Host "localhost"
                Port "2003"
                Protocol "tcp"
                LogSendErrors true
                Prefix "${cfg.prefix}."
                StoreRates true
                AlwaysAppendDS false
                EscapeCharacter "_"
              </Carbon>
            </Plugin>
            '';
          in "${pkgs.collectd}/sbin/collectd -C ${collectdConf} -f";
      };
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
    };
  };
}