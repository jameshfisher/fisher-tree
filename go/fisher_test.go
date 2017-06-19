package main

import "testing"
import "math/rand"
import "fmt"

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

func TestConst(*testing.T) {
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

func TestVar(*testing.T) {
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

func TestPropConstInsert(*testing.T) {
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

func TestPropConst(*testing.T) {
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

func TestPropVar(*testing.T) {
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
