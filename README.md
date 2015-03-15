# files-index
Files indexer and lookuper

# How to use
1. Index directory first:

`(files-index:index-dir "~/")`

2. Search with any valid lisp predicates using predefined files properties:
   1. `size` file size in bytes, `(files-index:to-bytes 5 :m)` can help you convert size.
   2. `type` is file extension. nil if file has no extension
   3. `path-tags` list of tags for file path
   4. `name-tags` list of tags for file name

`(files-index:lookup (null type))` ; finds files without extension

`(files-index:lookup (> size (files-index:to-bytes 5 :g)))` ; finds files larger than 5Gb

`(files-index:lookup (and (find "artist" name-tags :test 'equal) (equal type "part")))` ; find unfinished torrends with "artist" in name

3. Check file properties:

`(files-index:show-props "~/my-best-file.txt")`

4. Save indexed db:

`(with-open-file (*standard-output* "~/my-db.index" :direction :output)
    (files-index:dump-db))`

5. Restore indexed db:

`(with-open-file (in "~/my-db.index" :direction :input)
    (files-index:restore-db in))`

6. Goto step `2`

# TODO
* Add more files properties
 * creation time
 * modification time
 * access time
 * owner
 * group
 * permissions
 * etc

* Improve lookup language
 * easier lookup in tags
 * lookup part of tag
 * easier size conversion
 * limit results

* External interface
 * unix-like?
 * web?
 * 9P?

