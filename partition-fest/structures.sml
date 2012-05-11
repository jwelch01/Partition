structure Map = StringMap(TernaryKeyChar)
structure ListMap = ListMapFn(Map)
structure CharListMap = CharListMapFn(TernaryKeyChar)
structure M = EmbedListMap(structure L1 = Map
		           structure L2 = ListMap)
structure DB = TestDB(structure M1 = CharListMap
                      structure M2 = CharListMap
                      structure M3 = CharListMap)  
structure G = BasicGraph
structure TestCollection = TestCollection(Outcome)
structure SolnCollection = SolnCollection(Outcome)