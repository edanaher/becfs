set -ex

mkdir -p test
result/bin/becfs test
cd test
echo hi > hi
mkdir -p dir1/dir2/dir3
mkdir -p dir1/dir2.1
mkdir -p dir1/dir2.2
touch dir1/dir2.1/file2.1a
touch dir1/dir2.1/file2.1b
touch dir1/file1a
touch dir1/dir2/dir3/file3a
