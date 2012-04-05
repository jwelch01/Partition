structure Map = TernaryStringMap(TernaryKeyChar)
structure ListMap = ListMapFn(Map)
structure StringMap = StringMapFn(TernaryKeyChar)
structure M = EmbedListMap(structure L1 = Map
		           structure L2 = ListMap)
structure DB = TestDB(structure M1 = StringMap
                      structure M2 = StringMap
                      structure M3 = StringMap)  
structure G = BasicGraph
structure TestSet = TestSet(Outcome)
structure SolnSet = SolnSet(Outcome)