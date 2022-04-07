{ config, lib, pkgs, ... }: {
  programs.git = {
    userEmail =  "christophermcdevitt@gmail.com";
    signing = {
      key = "3A042C6B67C88936D05AD968288F081B1A54FB2A";
    };
  };
}
