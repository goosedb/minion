.SILENT:
beauty:
	fourmolu -i minion*/src minion*/app
	cabal-fmt -i minion*/*.cabal