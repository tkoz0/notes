# linux commands

How to do some common things with linux commands

### compress/decompress

Examples with `gzip`. Can replace with `bzip2` or `xz`.
- `gzip -k file` compresses `file` to `file.gz` keeping the original.
- `gzip -dk file.gz` decompresses `file.gz` to `file` keeping the original.
- `gzip > file.gz` compresses stdin to `file.gz`
- `gzip -dkc file.gz` decompresses `file.gz` to stdout

### seq

Sequences of integers.
- `seq 1 10` outputs integers 1,2,..,10, 1 per line
- `seq -f '%03g' 0 999` outputs 000,001,..,999 0 padded to width 3
- `seq -w 5 15` outputs numbers padded to equal length with 0's, like previous
