structure Map = TernaryStringMap(TernaryKeyChar)
structure ListMap = ListMapFn(Map)
structure M = EmbedMap(structure L1 = Map
		       structure L2 = ListMap)
structure G = BasicGraph
structure TestSet = TestSet(Outcome)
structure SolnSet = SolnSet(Outcome)