#!/usr/bin/env bash

TEMPLATE="${HOME}/dotfiles/templates"

read -p "Project Directory Name: " PROJECT
read -p "Project Package Name: " PKGNAME
read -p "Project Full Name: " FULLNAME

mkdir ${PROJECT}
cd ${PROJECT}

echo -e "# ${FULLNAME}\n\n"   > readme.md
cp ${TEMPLATE}/agpl3.license    LICENSE
cp ${TEMPLATE}/idris.gitignore  .gitignore

mkdir src

mkdir src/cbits
touch src/cbits/${PKGNAME}.h
touch src/cbits/${PKGNAME}.c

${TEMPLATE}/default_init.sh     ${PKGNAME}
${TEMPLATE}/nixpkg_init.sh      ${PKGNAME}
${TEMPLATE}/ipkg_init.sh        ${PKGNAME}
${TEMPLATE}/makefile_init.sh    ${PKGNAME}

git init
git add -A
git commit -m "initial commit for project ${PROJECT}"



