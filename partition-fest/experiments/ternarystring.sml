structure T = struct
  fun bind (x, y, z) = TernaryStringMap.bind (explode x, y, z)
  fun lookup (x, y) = TernaryStringMap.lookup(explode x, y)
end 