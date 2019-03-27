self: super: {

  idrisPackages = super.idrisPackages.override {
      overrides = self: super: {
        aatree   = self.callPackage /home/spydr/idrisDev/AA-Tree/aatree.nix {};
        pcgRand  = self.callPackage /home/spydr/idrisDev/PCGRand/pcgRand.nix {};
        terminal = self.callPackage /home/spydr/idrisDev/Terminal/terminal.nix {};
        testing  = self.callPackage /home/spydr/idrisDev/Testing/testing.nix {};

        #-- these ones are just for testing
        intmap   = self.callPackage /home/spydr/idrisDev/IntMap/intmap.nix {};
        treap    = self.callPackage /home/spydr/idrisDev/Treap/treap.nix {};
      };
  };

}


