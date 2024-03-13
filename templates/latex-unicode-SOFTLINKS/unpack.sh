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
  echo "  E.g., \`sh template/unpack.sh\`"
  exit 1
fi

# set -x

non_ow_copy () {
  if [ -f $2/$3 ]
  then
    echo "cp failed: file exists: $2/$3"
  else
    cp $1/$3 $2/$3
  fi
}

mkdir -p scripts sed tex tex/local img pdf

ln -s template/Makefile_darais .

(cd scripts && ln -s ../template/scripts/* ./)
(cd sed     && ln -s ../template/sed/* ./)
(cd tex     && ln -s ../template/tex/darais ./)

non_ow_copy template     .   .gitignore
non_ow_copy template     .   Makefile
non_ow_copy template/tex tex 00-main.tex
non_ow_copy template/tex tex local/defaults.tex
non_ow_copy template/tex tex local/imports.tex
non_ow_copy template/tex tex local/macros.tex
