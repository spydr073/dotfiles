self: super: {

  idrisPackages = super.idrisPackages.override {
      overrides = self: super: {
        aatree = self.callPackage /home/spydr/idrisDev/AA-Tree/aatree.nix {};
      };
  };

}


