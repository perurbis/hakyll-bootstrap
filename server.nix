let

  region = "eu-west-1";
  accessKeyId = "cdodev"; # symbolic name looked up in ~/.ec2-keys

  ec2 =
   { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "t2.micro";
      deployment.ec2.elasticIPv4 = "52.49.42.182";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
      deployment.ec2.securityGroups = ["allow-ssh" "allow-http"];
      
      imports = [
        ./modules/collectd.nix
        ./modules/logster.nix
        ./modules/nginx.nix
      ];

      services.collectd.prefix = "cdodev";
      services.logster = {
        enable = true;
        graphiteHost = "localhost:2003";
        prefix = "cdodev.nginx";
      };
    };

  
in
{
  network.description = "Commando Development nginx";
  network.enableRollback = true;

  site = ec2;
  # Provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };

}