package main

import "fmt"
import "math/rand"

func assert(b bool) {
  if !b {
    panic("Assertion failed");
  }
}

func assert_eq(a interface{}, b interface{}) {
  if a != b {
    fmt.Printf("%#v != %#v\n", a, b);
    panic("Not equal");
  }
}

func assert_eq_arr(a []byte, b []byte) {
  assert_eq(len(a), len(b));
  for i := 0; i < len(a); i++ {
    assert_eq(a[i], b[i]);
  }
}

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

func assert_fisher_valid_nonempty(_tree interface{}, height int) {
  if 0 < height {
    var tree *fisher_node = _tree.(*fisher_node);
    assert_eq(tree.height, height);
    assert(len(tree.prefix) <= height);  // Prefix can be zero-length.
    var after_prefix_height int = height - len(tree.prefix);
    if after_prefix_height == 0 {
      assert_eq(len(tree.branches), 0);
    } else {
      assert(1 < len(tree.branches));
      assert(len(tree.branches) <= 256);
      var after_prefix_and_branches_height int = after_prefix_height-1;
      for _, t := range tree.branches {
        assert_fisher_valid_nonempty(t, after_prefix_and_branches_height);
      }
    }
  }
}

func assert_fisher_valid(tree interface{}, height int) {
  if tree != nil {
    assert_fisher_valid_nonempty(tree, height);
  }
}

func fisher_find(_tree interface{}, key []byte) interface{} {
  // fmt.Printf("fisher_find(%#v, %#v)\n", _tree, key);
  assert_fisher_valid(_tree, len(key));
  if len(key) == 0 {
    return _tree;
  }
  if _tree == nil {
    return nil;
  }
  var tree *fisher_node = _tree.(*fisher_node);
  assert_eq(tree.height, len(key));
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
  assert_fisher_valid(_tree, len(key));
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
  assert_fisher_valid(_tree, len(key));
  if len(key) == 0 {
    return nil;
  }
  if _tree == nil {
    return nil;
  }
  var tree *fisher_node = _tree.(*fisher_node);
  assert_eq(tree.height, len(key));
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
    assert(1 < len(tree.branches));
    delete(tree.branches, key[len(prefix)]);  // delete any associated value.
    if 1 < len(tree.branches) {
      return tree;
    }
    assert_eq(1, len(tree.branches));
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
  assert(1 < len(tree.branches));
  delete(tree.branches, key[len(prefix)]);
  if 1 < len(tree.branches) {
    return tree;
  }
  assert_eq(1, len(tree.branches));
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
  assert_fisher_valid(t, len(key));
  return fisher_find(t, key);
}

func fisher_v_insert(len_map interface{}, key []byte, val interface{}) interface{} {
  return fisher_insert(len_map, int_bytes(len(key)), fisher_insert(fisher_find(len_map, int_bytes(len(key))), key, val));
}

func fisher_v_delete(len_map interface{}, key []byte) interface{} {
  var t interface{} = fisher_find(len_map, int_bytes(len(key)));
  assert_fisher_valid(t, len(key));
  var new_t interface{} = fisher_delete(t, key);
  if new_t == nil {
    return fisher_delete(len_map, int_bytes(len(key)));
  } else {
    return fisher_insert(len_map, int_bytes(len(key)), new_t);
  }
}

func test_const() {
  t := fisher_empty();
  assert_eq(fisher_find(t, []byte{1,2,3}), nil);

  t = fisher_insert(t, []byte{4,5,6}, "foo");
  assert_fisher_valid_nonempty(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), nil);
  assert_eq(fisher_find(t, []byte{4,5,6}), "foo");

  t = fisher_insert(t, []byte{4,5,7}, "bar");
  assert_fisher_valid_nonempty(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), nil);
  assert_eq(fisher_find(t, []byte{4,5,6}), "foo");
  assert_eq(fisher_find(t, []byte{4,5,7}), "bar");

  t = fisher_insert(t, []byte{1,2,3}, "123");
  assert_fisher_valid_nonempty(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), "123");
  assert_eq(fisher_find(t, []byte{4,5,6}), "foo");
  assert_eq(fisher_find(t, []byte{4,5,7}), "bar");

  t = fisher_insert(t, []byte{1,2,3}, "999");
  assert_fisher_valid_nonempty(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), "999");
  assert_eq(fisher_find(t, []byte{4,5,6}), "foo");
  assert_eq(fisher_find(t, []byte{4,5,7}), "bar");

  t = fisher_delete(t, []byte{7,8,9});
  assert_fisher_valid_nonempty(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), "999");
  assert_eq(fisher_find(t, []byte{4,5,6}), "foo");
  assert_eq(fisher_find(t, []byte{4,5,7}), "bar");

  t = fisher_delete(t, []byte{4,5,6});
  assert_fisher_valid_nonempty(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), "999");
  assert_eq(fisher_find(t, []byte{4,5,6}), nil);
  assert_eq(fisher_find(t, []byte{4,5,7}), "bar");

  t = fisher_delete(t, []byte{4,5,7});
  assert_fisher_valid_nonempty(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), "999");
  assert_eq(fisher_find(t, []byte{4,5,6}), nil);
  assert_eq(fisher_find(t, []byte{4,5,7}), nil);

  t = fisher_delete(t, []byte{1,2,3});
  assert_fisher_valid(t, 3);
  assert_eq(fisher_find(t, []byte{1,2,3}), nil);
  assert_eq(fisher_find(t, []byte{4,5,6}), nil);
  assert_eq(fisher_find(t, []byte{4,5,7}), nil);
}

func test_var() {
  assert_eq_arr(int_bytes(17), []byte{17,0,0,0,0,0,0,0});
  assert_eq_arr(int_bytes(257), []byte{1,1,0,0,0,0,0,0});

  t := fisher_v_empty();
  t = fisher_v_insert(t, []byte{1,2,3}, "123");
  assert_eq(fisher_v_find(t, []byte{}), nil);
  assert_eq(fisher_v_find(t, []byte{1,2,3}), "123");
  t = fisher_v_insert(t, []byte{}, "empty");
  assert_eq(fisher_v_find(t, []byte{1,2,3}), "123");
  assert_eq(fisher_v_find(t, []byte{}), "empty");

  t = fisher_v_delete(t, []byte{});
  assert_eq(fisher_v_find(t, []byte{1,2,3}), "123");
  assert_eq(fisher_v_find(t, []byte{}), nil);

  t = fisher_v_delete(t, []byte{1,2,3});
  assert_eq(fisher_v_find(t, []byte{1,2,3}), nil);
  assert_eq(fisher_v_find(t, []byte{}), nil);
}

func rand_bytes_3() [3]byte {
  var bytes []byte = []byte{0,1,2,7,255};
  var rand_bytes [3]byte;
  for j := 0; j < 3; j++ {
    var rand_byte byte = bytes[rand.Int() % len(bytes)];
    rand_bytes[j] = rand_byte;
  }
  return rand_bytes;
}

func rand_bytes_v() []byte {
  var byte_lengths []int = []int{0,1,2,255,256,257};
  var byte_length int = byte_lengths[rand.Int() % len(byte_lengths)];

  var bytes []byte = []byte{0,1,255};

  var rand_bytes []byte;
  for j := 0; j < byte_length; j++ {
    var rand_byte byte = bytes[rand.Int() % len(bytes)];
    rand_bytes = append(rand_bytes, rand_byte);
  }
  return rand_bytes;
}

func prop_test_const_insert() {
  fmt.Printf("prop_test_const_insert\n");

  var t interface{} = fisher_empty();
  var control map[[3]byte]interface{} = map[[3]byte]interface{}{};

  for i := 0; i < 10000; i++ {
    var rand_insert_key [3]byte = rand_bytes_3();
    var rand_insert_val string = fmt.Sprintf("%#v", rand_insert_key);
    // fmt.Printf("Inserting %#v -> %#v\n", rand_insert_key, rand_insert_val);
    t = fisher_insert(t, rand_insert_key[:], rand_insert_val);
    // fmt.Printf("Insert yields tree: %#v\n", t);
    control[rand_insert_key] = rand_insert_val;

    var rand_test_key [3]byte = rand_bytes_3();
    assert_eq(fisher_find(t, rand_test_key[:]), control[rand_test_key]);

    assert_fisher_valid_nonempty(t, 3);
  }
}

func prop_test_const() {
  fmt.Printf("prop_test_const\n");

  var t interface{} = fisher_empty();
  var control map[[3]byte]interface{} = map[[3]byte]interface{}{};

  for i := 0; i < 100000; i++ {
    switch rand.Int() % 3 {
      case 0:
        var rand_insert_key [3]byte = rand_bytes_3();
        var rand_insert_val string = fmt.Sprintf("%#v", rand_insert_key);
        // fmt.Printf("Inserting %#v -> %#v\n", rand_insert_key, rand_insert_val);
        t = fisher_insert(t, rand_insert_key[:], rand_insert_val);
        // fmt.Printf("Insert yields tree: %#v\n", t);
        control[rand_insert_key] = rand_insert_val;

        assert_fisher_valid_nonempty(t, 3);

      case 1:
        var rand_delete_key [3]byte = rand_bytes_3();
        t = fisher_delete(t, rand_delete_key[:]);
        delete(control, rand_delete_key);

        assert_fisher_valid(t, 3);

      case 2:
        var rand_test_key [3]byte = rand_bytes_3();
        assert_eq(fisher_find(t, rand_test_key[:]), control[rand_test_key]);
    }

    if (i % 1000 == 0) { fmt.Printf("Done %d\n", i); }
  }
}

func prop_test_var() {
  fmt.Printf("prop_test_var\n");

  var t interface{} = fisher_v_empty();
  var control map[string]interface{} = map[string]interface{}{}; // string == HACK

  for i := 0; i < 10000; i++ {
    switch rand.Int() % 3 {
      case 0:
        var rand_insert_key []byte = rand_bytes_v();
        var rand_insert_val string = fmt.Sprintf("%#v", rand_insert_key);
        // fmt.Printf("Inserting %#v -> %#v\n", rand_insert_key, rand_insert_val);
        t = fisher_v_insert(t, rand_insert_key, rand_insert_val);
        // fmt.Printf("Insert yields tree: %#v\n", t);
        control[fmt.Sprintf("%#v", rand_insert_key)] = rand_insert_val;

        // assert_fisher_valid_nonempty(t, 3);

      case 1:
        var rand_delete_key []byte = rand_bytes_v();
        t = fisher_v_delete(t, rand_delete_key);
        delete(control, fmt.Sprintf("%#v", rand_delete_key));

        // assert_fisher_valid(t, 3);

      case 2:
        var rand_test_key []byte = rand_bytes_v();
        assert_eq(fisher_v_find(t, rand_test_key[:]), control[fmt.Sprintf("%#v", rand_test_key)]);
    }

    if (i % 1000 == 0) { fmt.Printf("Done %d\n", i); }
  }
}

func main() {
  test_const();
  test_var();

  prop_test_const_insert();
  prop_test_const();

  prop_test_var();
}
