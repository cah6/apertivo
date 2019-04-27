{
  network.description = "Happy hour servers";

  backend1 =
    { config, pkgs, ... }: 
    let
      backend = (import ./backend.nix {}).backend;
    in
    { 
      networking.hostName = "backend1";

      networking.firewall.allowedTCPPorts = [ 22 80 3000 ];
      environment.systemPackages = [ backend ];

      systemd.services.backend =
        { 
          description = "Happy Hour Webserver";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${backend}/bin/backend";
            };
          environment = {
            ES_PASSWORD = builtins.readFile ./es_password.secret;
          };
        };
    };
}
