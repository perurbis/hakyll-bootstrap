{ config, pkgs, ... }:
{
  services.nginx = {
    enable = true;
    config =
      ''
        events {
          worker_connections 1024;
          use epoll;
        }
      '';
    httpConfig = ''
      include ${pkgs.nginx}/conf/mime.types;

      gzip on;
      gzip_http_version 1.1;
      gzip_min_length 1100;
      gzip_vary on;
      gzip_comp_level 2;
      gzip_types text/plain text/html text/css
                 application/x-javascript text/xml
                 application/xml application/xml+rss
                 text/javascript application/json;

      proxy_cache_key $scheme$host$request_uri;
      proxy_cache_path ${config.services.nginx.stateDir} levels=2:2
      keys_zone=global:64m inactive=60m max_size=1G;
    '';
  };
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}