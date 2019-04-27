let
  region = "us-east-1";
  accessKeyId = "hhkey";
in
{ 
  backend1 = { resources, ... }:
    { 
      deployment.targetEnv = "ec2";
      deployment.ec2 = {
        inherit region accessKeyId;
        subnetId = "subnet-5bf9af77";
        instanceType = "t2.micro";
        associatePublicIpAddress = true;
        keyPair = "my-free-tier";
        privateKey = "~/.ssh/my-free-tier.pem";
        ebsInitialRootDiskSize = 20;
      };
    };
}