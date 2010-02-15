# Compiles sbcl 1.0.35 with threads enabled on OSX.
# Since sbcl dependes on sbcl, 1.0.30 binary is used to compile it.
#
# Usage: $0 install-path
#
# Known to work on Mac OSX 10.6.2
set -e

DOWNLOAD=/tmp/weblocks-install
# clears out SBCL_HOME, in case it is already set in the env already.  SBCL binary install will stop if it's already set.
SBCL_HOME="" 

if [ -z "$1" ]
then
  echo "Usage: $0 install-path"
  exit 1
fi

USR=`cd $1; pwd`

set -u
# set -x # debug mode

test -d "$DOWNLOAD" || mkdir "$DOWNLOAD"

if [ ! -d "$USR/sbcl-1.0.30-binary" ]
then
  echo -e "\033[32m Downloading sbcl-1.0.30 binary to $DOWNLOAD \033[0m"
  test -f "$DOWNLOAD"/sbcl-1.0.30-x86-darwin-binary.tar.bz2 || curl -C - -o "$DOWNLOAD"/sbcl-1.0.30-x86-darwin-binary.tar.bz2 -L http://sourceforge.net/projects/sbcl/files/sbcl/1.0.30/sbcl-1.0.30-x86-darwin-binary.tar.bz2/download

  test -d "$DOWNLOAD"/sbcl-1.0.30-x86-darwin-binary || tar -xvf "$DOWNLOAD"/sbcl-1.0.30-x86-darwin-binary.tar.bz2 -C "$DOWNLOAD"

  echo -e "\033[36m Installing sbcl-1.0.30 binary in $USR \033[0m"
  cd "$DOWNLOAD"/sbcl-1.0.30-x86-darwin
  INSTALL_ROOT="$USR"/sbcl-1.0.30-binary sh install.sh
  echo -e "\033[36m Installed sbcl-1.0.30 binary sucessfully \033[0m"
  cd -
else
  echo -e "\033[1m sbcl-1.0.30 binary is already installed - skipping \033[0m"
fi

if [ ! -d "$USR"/sbcl-1.0.35 ]
then
  # so SBCL can find the location of the core file for version .30
  SBCL_HOME="$USR"/sbcl-1.0.30-binary/lib/sbcl
  PATH=$PATH:"$USR"/sbcl-1.0.30-binary/bin

  echo -e "\033[32m Downloading sbcl-1.0.35 from source to $DOWNLOAD \033[0m"
  test -f "$DOWNLOAD"/sbcl-1.0.35-source.tar.bz2 || curl -C - -o "$DOWNLOAD"/sbcl-1.0.35-source.tar.bz2 -L http://sourceforge.net/projects/sbcl/files/sbcl/1.0.35/sbcl-1.0.35-source.tar.bz2/download
  test -d "$DOWNLOAD"/sbcl-1.0.35-source.tar.bz2 || tar -xvf "$DOWNLOAD"/sbcl-1.0.35-source.tar.bz2 -C "$DOWNLOAD"

  echo -e "\033[36m Installing sbcl-1.0.35 from source in $USR \033[0m"
  cd "$DOWNLOAD"/sbcl-1.0.35

  cat << EOF > customize-target-features.lisp
(lambda (features)
  (flet ((enable (x) 
         (pushnew x features))
         (disable (x) 
           (setf features (remove x features))))
        ;; Threading support, available only on x86/x86-64 Linux, x86 Solaris
        ;; and x86 Mac OS X (experimental).
    (enable :sb-thread)))
EOF

  sh make.sh
  cd -
  cd "$DOWNLOAD"/sbcl-1.0.35/tests
  cd tests && sh run-tests.sh
  cd -
  cd "$DOWNLOAD"/sbcl-1.0.35/doc
  cd doc/manual && make
  cd -
  cd "$DOWNLOAD"/sbcl-1.0.35

  SBCL_HOME=""
  INSTALL_ROOT="$USR"/sbcl-1.0.35 sh install.sh

  echo -e "\033[36m Installed sbcl-1.0.35 sucessfully \033[0m"
  cd -
else
  echo -e "\033[1m sbcl-1.0.35 is already installed - skipping \033[0m"
fi

rm -rf "$DOWNLOAD"

echo "Please setup SBCL_HOME=$USR/sbcl-1.0.35/lib/sbcl variable in your .bashrc or .bash_profile "
echo "and add $USR/sbcl-1.0.35/bin to your PATH variable"
