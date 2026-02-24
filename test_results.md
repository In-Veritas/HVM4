# HVM4 Test Results

Generated: 2026-02-24 16:53:25

## Modes

Each program is executed in four modes to verify consistent results:

| Mode | Flags | Description |
|------|-------|-------------|
| **SNF 1T** | `-s` | Strong Normal Form, single-threaded. Fully reduces the term, keeping SUPs and DUPs in the output. |
| **SNF MT** | `-s -T10` | Strong Normal Form, multi-threaded (10 workers). Same reduction as SNF 1T but parallelized. |
| **CNF 1T** | `-s -C` | Collapsed Normal Form, single-threaded. Eliminates SUPs/DUPs and enumerates all branches as pure lambda terms. |
| **CNF MT** | `-s -C -T10` | Collapsed Normal Form, multi-threaded (10 workers). Same collapse as CNF 1T but parallelized. |

A test **passes** when all four modes produce the same output (after sorting, since collapse branch order is non-deterministic).

## Results

| Program | SNF 1T | SNF MT | CNF 1T | CNF MT | Match |
|---------|--------|--------|--------|--------|-------|
| `append_list` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| `list_min_const` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| `list_min_num` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| `list_pop` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| `select_sort` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| `smaller_letter` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| `string_id_first` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |

## Output Details

### `append_list`

All 4 modes produced identical output:

```
[1,2,3,4,5,6,7,8,9]
```

---

### `list_min_const`

All 4 modes produced identical output:

```
#SOME{2}
```

---

### `list_min_num`

All 4 modes produced identical output:

```
2
```

---

### `list_pop`

All 4 modes produced identical output:

```
[1,2,4,5]
```

---

### `select_sort`

All 4 modes produced identical output:

```
[1,2,2,2,3,3,4,4,5,5,6]
```

---

### `smaller_letter`

All 4 modes produced identical output:

```
'a'
```

---

### `string_id_first`

All 4 modes produced identical output:

```
97
```

---

