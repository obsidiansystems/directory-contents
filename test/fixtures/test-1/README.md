This test directory tree has the following structure:
```
.
├── a -> A/a
├── A
│   ├── a
│   ├── A -> ../A  [recursive, not followed]
│   └── B -> ../B
│       ├── A -> ../A  [recursive, not followed]
│       └── b
├── B
│   ├── A -> ../A  [recursive, not followed]
│   └── b
└── README.md

6 directories, 5 files
```
