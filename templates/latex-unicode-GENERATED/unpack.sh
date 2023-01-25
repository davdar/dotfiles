#! /bin/sh
#
# One way to setup a fresh project is to:
#
# - create a new project directory, say `project`
# - copy this directory into `project/template`
# - from `project`, run `sh template/unpack.sh <tex_src_dir>`
#
# This will soft link files that shouldn't be changed to point to files in the
# template, allowing easy updates to the template, and make the template
# directory read-only, to prevent accidental modification of the template.

if [ "$(dirname $0)" != "template" ]
then 
  echo "ERROR"
  echo "  unpack.sh must be called from its parent directory."
  echo "  E.g., \`sh template/unpack.sh <tex_src_dir>\`"
  exit 1
fi

if [ "$#" -ne 1 ]
then
  echo "ERROR"
  echo "  unpack.sh must be called with one argument:"
  echo "  the destination for *.tex source files."
  echo "  E.g., \`sh template/unpack.sh <tex_src_dir>\`"
  exit 1
fi

# set -x

non_ow_copy () {
  if [ -f $2/$3 ]
  then
    echo "cp: $2/$3: File exists"
    # echo "WOULD NOT DO: cp $1/$3 $2/$3"
  else
    # echo "WOULD DO: cp $1/$3 $2/$3"
    cp $1/$3 $2/$3
  fi
}

mkdir -p scripts sed $1 $1/local

ln -s $PWD/template/Makefile_darais .

ln -s $PWD/template/scripts/* scripts
ln -s $PWD/template/sed/* sed
ln -s $PWD/template/tex/darais $1

non_ow_copy $PWD/template     .  .gitignore
non_ow_copy $PWD/template     .  Makefile
non_ow_copy $PWD/template/tex $1 00-main.tex
non_ow_copy $PWD/template/tex $1 local/defaults.tex
non_ow_copy $PWD/template/tex $1 local/imports.tex
non_ow_copy $PWD/template/tex $1 local/macros.tex

chmod 555 $PWD/template
