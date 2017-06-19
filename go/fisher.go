package main

// Represents a non-empty map!
// To represent an empty map, use a null pointer.
type fisher_node struct {
  height int;  // Unnecessary, but useful for assertions

  prefix []byte;

  // In C we will use an undiscriminated union for val/branches because exactly one will be present.
  // In C, this will follow the prefix in memory.
  val interface{};
  branches map[byte]interface{};  // In C we will use two concatted arrays and binary search; not a hash table.
}

func fisher_find(_tree interface{}, key []byte) interface{} {
  // fmt.Printf("fisher_find(%#v, %#v)\n", _tree, key);
  // assert_fisher_valid(_tree, len(key));
  if len(key) == 0 {
    return _tree;
  }
  if _tree == nil {
    return nil;
  }
  var tree *fisher_node = _tree.(*fisher_node);
  // assert_eq(tree.height, len(key));
  var prefix []byte = tree.prefix;
  var i int = 0;
  for i < len(prefix) {
    if prefix[i] != key[i] {
      return nil;
    }
    i++;
  }
  remaining := len(key) - len(prefix);
  if remaining == 0 {
    return tree.val;
  }
  next, ok := tree.branches[key[len(prefix)]];
  if !ok {
    return nil;
  }
  return fisher_find(next, key[len(prefix)+1:]);
}

func fisher_singleton(key []byte, val interface{}) interface{} {
  if len(key) == 0 {
    return val;
  }
  return &fisher_node {
    height: len(key),
    prefix: key,
    val: val,
    branches: map[byte]interface{}{},
  };
}

func fisher_empty() interface{} {
  return nil;
}

func fisher_insert(_tree interface{}, key []byte, val interface{}) interface{} {
  // assert_fisher_valid(_tree, len(key));
  if len(key) == 0 {
    return val;
  }

  if _tree == nil {
    return fisher_singleton(key, val);
  }
  var tree *fisher_node = _tree.(*fisher_node);

  var prefix []byte = tree.prefix;
  var i int = 0;
  for i < len(prefix) {
    if prefix[i] != key[i] {
      // Gotta split. We get a new binary node, one branch for each of prefix[i] and key[i].
      // prefix[i] maps to the existing stuff, key[i] maps to new singleton branch.
      var sibling interface{}
      if len(prefix) == len(key) && i+1 == len(key) {
        // We failed on the very last byte!
        sibling = tree.val;
      } else {
        sibling = &fisher_node {
          height: tree.height - (i+1),
          prefix: prefix[i+1:],
          val: tree.val,
          branches: tree.branches,
        };
      }
      var new_leaf interface{} = fisher_singleton(key[i+1:], val);
      return &fisher_node {
        height: tree.height,
        prefix: prefix[:i],
        val: nil,
        branches: map[byte]interface{}{
          prefix[i]: sibling,
          key[i]: new_leaf,
        },
      };
    }
    i++;
  }
  remaining := len(key) - len(prefix);
  if remaining == 0 {
    tree.val = val;
    return tree;
  }
  next, ok := tree.branches[key[len(prefix)]];
  if !ok {
    tree.branches[key[len(prefix)]] = fisher_singleton(key[len(prefix)+1:], val);
    return tree;
  }

  tree.branches[key[len(prefix)]] = fisher_insert(next, key[len(prefix)+1:], val);

  return tree;
}

func fisher_delete(_tree interface{}, key []byte) interface{} {
  // assert_fisher_valid(_tree, len(key));
  if len(key) == 0 {
    return nil;
  }
  if _tree == nil {
    return nil;
  }
  var tree *fisher_node = _tree.(*fisher_node);
  // assert_eq(tree.height, len(key));
  var prefix []byte = tree.prefix;
  var i int = 0;
  for i < len(prefix) {
    if prefix[i] != key[i] {
      return tree;
    }
    i++;
  }
  remaining := len(key) - len(prefix);
  if remaining == 0 {
    // delete this node!
    return nil;
  }
  next, ok := tree.branches[key[len(prefix)]];
  if !ok {
    return tree;
  }

  remaining -= 1;

  if remaining == 0 {
    // tree is a leaf.
    // assert(1 < len(tree.branches));
    delete(tree.branches, key[len(prefix)]);  // delete any associated value.
    if 1 < len(tree.branches) {
      return tree;
    }
    // assert_eq(1, len(tree.branches));
    // merge the prefix and the single branch byte.
    var b byte;
    var v interface{};
    for b, v = range tree.branches {};
    tree.prefix = append(prefix, b);
    tree.val = v;
    tree.branches = map[byte]interface{}{};
    return tree;
  }

  // tree is a non-leaf branch.
  new_subtree := fisher_delete(next, key[len(prefix)+1:]);

  if new_subtree != nil {
    tree.branches[key[len(prefix)]] = new_subtree;
    return tree;
  }

  // We got a nil subtree back; delete it from the branches.
  // assert(1 < len(tree.branches));
  delete(tree.branches, key[len(prefix)]);
  if 1 < len(tree.branches) {
    return tree;
  }
  // assert_eq(1, len(tree.branches));
  // merge the prefix, the single branch byte, and the following node.
  var b byte;
  var sub interface{};
  for b, sub = range tree.branches {};
  var sub_fisher *fisher_node = sub.(*fisher_node);

  sub_fisher.height = tree.height;
  sub_fisher.prefix = append(append(prefix, b), sub_fisher.prefix...);
  return sub_fisher;
  // free(tree);
}

// In C, this would just be a cast, i.e. `&x, sizeof(x)`, not a runtime operation
func int_bytes(x int) []byte {
  var bytes = []byte{};
  for (0 < x) {
    bytes = append(bytes, byte(x % 256));
    x = x / 256;
  }
  for len(bytes) < 8 {
    bytes = append(bytes, 0);
  }
  return bytes;
}

func fisher_v_empty() interface{} {
  return fisher_empty();
}

func fisher_v_find(len_map interface{}, key []byte) interface{} {
  var t interface{} = fisher_find(len_map, int_bytes(len(key)));
  // assert_fisher_valid(t, len(key));
  return fisher_find(t, key);
}

func fisher_v_insert(len_map interface{}, key []byte, val interface{}) interface{} {
  return fisher_insert(len_map, int_bytes(len(key)), fisher_insert(fisher_find(len_map, int_bytes(len(key))), key, val));
}

func fisher_v_delete(len_map interface{}, key []byte) interface{} {
  var t interface{} = fisher_find(len_map, int_bytes(len(key)));
  // assert_fisher_valid(t, len(key));
  var new_t interface{} = fisher_delete(t, key);
  if new_t == nil {
    return fisher_delete(len_map, int_bytes(len(key)));
  } else {
    return fisher_insert(len_map, int_bytes(len(key)), new_t);
  }
}
