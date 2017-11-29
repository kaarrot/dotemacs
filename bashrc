# set speed of the yoda pointer
# overall speed of the pointer

# polynomial profile http://www.x.org/wiki/Development/Documentation/PointerAcceleration/#index8h3
# xinput set-prop "SONiX USB Device" "Device Accel Profile" 2
# xinput set-prop "SONiX USB Device" "Device Accel Constant Deceleration" 4
# xinput set-prop "SONiX USB Device" "Device Accel Adaptive Deceleration" 10
# 10 is the default value
# xinput set-prop "SONiX USB Device" "Device Accel Velocity Scaling" 10.0
#                                          threshold num denom
# xinput set-ptr-feedback "SONiX USB Device" 100         30   10

# ~/.xsessionrc
# ~/.xinitrc
# ~/.xprofile
#xinput --list-props "SONiX USB Device" 

# PS1="$PS1\n> "
export PS1="[\A]\u@\h \w\\n$ "

alias u='cd ..; ls'
alias ee='emacs -nw'
alias sub='$HOME/bin/sublime_text_3/sublime_text -n .'
alias EDITOR='emacs'
alias subl='$HOME/bin/sublime_text_3/sublime_text -n . &'
alias um='micro'
alias he='hescape'
alias hsource='cd /opt/hfs13.0.547/; source houdini_setup_bash'
alias gogit='cd ~/PRJ/GIT'
alias gosrc='cd ~/SRC'
alias goprj='cd /home/kuba/PRJ'

export PATH=/home/kuba/bin:/home/kuba/bin/emacs/bin:/usr/local/bin/:/home/kuba/bin/nim/bin:/home/kuba/.nimble/bin:$PATH
export PATH=/home/kuba/bin/kak/usr/local/bin:$PATH
export PATH=$HOME/toolchains/clang50/bin/:$PATH
export PATH=$HOME/toolchains/qt510/bin:$HOME/toolchains/bin:$PATH

# Golang
export GOROOT=$HOME/toolchains/go
export PATH=$HOME/toolchains/go/bin:$PATH
export GOPATH=$HOME/PRJ/go

#export PYTHONPATH=$PYTHONPATH:/home/kuba/SRC/PyPDF2


# Grumpy
env_grumpy(){
  cd $HOME/SRC/grumpy
  make
  export GOPATH=$PWD/build
  export PYTHONPATH=$PWD/build/lib/python2.7/site-packages
  
  echo "--------"
  echo "echo 'print \"hello, world\"' > hello.py"
  echo "tools/grumpc hello.py > hello.go"
  echo "go build -o hello hello.go"
}

# this for the racer completion
#alias cmake='/home/kuba/SRC/clion/bin/cmake/bin/cmake'
alias hist='history $1'

#OCaml
# eval `opam config env`

# Rust
export PATH=/home/kuba/toolchains/rust/cargo/bin:/home/kuba/toolchains/rust/rustc/bin:$PATH
## Point to std on ubuntu
export RUSTFLAGS="-L /home/kuba/toolchains/rust/rust-std-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib"

## Clang 6
export PATH=/home/kuba/toolchains/llvm/bin:$PATH
export LD_LIBRARY_PATH=/home/kuba/toolchains/llvm/lib:$LD_LIBRARY_PATH

# export LD_LIBRARY_PATH=/home/kuba/toolchains/lib:$LD_LIBRARY_PATH
### RACER completin stuff
### before: cargo install racer
export RUST_SRC_PATH=/home/kuba/SRC/rust/src/
export PATH=$PATH:/home/kuba/.cargo/bin

complete -f  ee


source ~/git-completion.bash
#git
alias g-l='git log --pretty=oneline --abbrev-commit'
alias g-b='git branch -vv'
alias g-c='git checkout'
alias g-s='git status'
alias g-d='git diff --name-only'
alias g-r='git for-each-ref --sort=committerdate refs/heads/'
alias g-prev='git reset --hard `git log -n 1 --skip 1 --format="%H"`'

alias julia-svn='/home/kuba/SRC/julia-svn/usr/bin/julia'
alias julia3='/home/kuba/julia3.11/bin/julia'
alias ijulia='ipython notebook --prefile=julia'

alias clion='/home/kuba/SRC/clion/bin/clion.sh'


alias goocm='cd /home/kuba/PRJ/0_lectures/OCM/'
alias ipy='goocm; ipython notebook'

copy-from-git(){
    rsync  -avz --exclude '*.git*' --exclude '.idea' $1/ $2
}

copy-from-git(){
    rsync  -avz --exclude '*.git*' $1/ $2
}

#OCaml
. /home/kuba/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# codeblocks and other default stuff
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

juliaExec(){
  rm /home/kuba/bin/julia0.4/bin/../share/julia/base/userimg.jl
  rm /home/kuba/bin/julia0.4/lib/julia/libout_julia.ji
  julia /home/kuba/SRC/julia/contrib/build_executable.jl out_julia $1 
}

# env_julia (){
#     export LD_LIBRARY_PATH=/home/kuba/toolchains/lib:/home/kuba/toolchains/lib64:$LD_LIBRARY_PATH
#     export PATH=/home/kuba/toolchains/bin:$PATH

#     export CC=/home/kuba/toolchains/bin/gcc
#     export CXX=/home/kuba/toolchains/bin/g++
#     export FC=/home/kuba/toolchains/bin/gfortran
#     alias cmake='/data/app/sci6_x86_64/CMake/2.8.10.2/cmake/bin/cmake'

# }

env_gcc49 (){
    export CC=/usr/bin/gcc-4.9;
    export CPP=/usr/bin/cpp-4.9;
    export CXX=/usr/bin/gcc-4.9;
    export CCC=/usr/bin/gcc-4.9;
    export LD=/usr/bin/gcc-4.9;
    
    alias gcc=/usr/bin/gcc-4.9;
    alias cpp=/usr/bin/cpp-4.9;
    alias g++=/usr/bin/cpp-4.9;
    alias ld=/usr/bin/gcc-4.9;
    alias cc=/usr/bin/gcc-4.9;

    # export LD_LIBRARY_PATH=/home/kuba/toolchains/gcc492/lib64:/home/kuba/toolchains/gcc492/lib
    export PATH=$HOME/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH
    export PATH=$HOME/toolchains/qt510/bin:$HOME/toolchains/bin:$PATH

}

env_houdini (){
    export CC=/usr/bin/gcc-4.9;
    export CPP=/usr/bin/cpp-4.9;
    export CXX=/usr/bin/gcc-4.9;
    export CCC=/usr/bin/gcc-4.9;
    export LD=/usr/bin/gcc-4.9;
    
    alias gcc=/usr/bin/gcc-4.9;
    alias cpp=/usr/bin/cpp-4.9;
    alias g++=/usr/bin/cpp-4.9;
    alias ld=/usr/bin/gcc-4.9;
    alias cc=/usr/bin/gcc-4.9;

    # export LD_LIBRARY_PATH=/home/kuba/toolchains/gcc492/lib64:/home/kuba/toolchains/gcc492/lib
    export PATH=$HOME/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
    export PATH=$HOME/toolchains/qt59/bin:$HOME/toolchains/bin:$PATH
  
    cd /opt/hfs16.0
    source ./houdini_setup_bash
}

env_clang (){
    export LD_LIBRARY_PATH=/home/kuba/toolchains/llvm/lib:$LD_LIBRARY_PATH
    export INCLUDE_PATH=/home/kuba/toolchains/llvm/include:$INCLUDE_PATH
    export PATH=/home/kuba/toolchains/llvm/bin/:$PATH
    
    export CC=/home/kuba/toolchains/llvm/bin/clang;
    export CPP=/home/kuba/toolchains/llvm/bin/clang-cpp;
    export CXX=/home/kuba/toolchains/llvm/bin/clang++;
    export CCC=/home/kuba/toolchains/llvm/bin/clang++;
    export LD=/home/kuba/toolchains/llvm/bin/llvm-link;
    
    alias cc=/home/kuba/toolchains/llvm/bin/clang;
    alias gcc=/home/kuba/toolchains/llvm/bin/clang;
    alias cpp=/home/kuba/toolchains/llvm/bin/clang-cpp;
    alias g++=/home/kuba/toolchains/llvm/bin/clang++;
    alias ld=/home/kuba/toolchains/llvm/bin/llvm-link;   
}

# env_qt (){
#     export LD_LIBRARY_PATH=/home/kuba/toolchains/qt560/lib:/home/kuba/toolchains/lib:$LD_LIBRARY_PATH
#     export PATH=/home/kuba/toolchains/qt560/bin/:$PATH
# }



copy-from-git(){
   rsync  -avz --exclude '*.git*' --exclude '.idea' $1/ $2
}

copy-from-git(){
   rsync  -avz --exclude '*.git*' $1/ $2
}

function title {
    echo -ne "\033]0;"$*"\007"
}

function lsl() { 
# list recent number of directiores, 
# if the number is skipped the last one is returnd
# This commnad cna be chained with 'cd' to change the dir.
# For example: cd `lsl` which is eqivalent of cd `lsl 1`
if [ -z $1 ]; then
    local num=1;
else
    local num=$1
fi
ls -dt * | head -$num
}

function cdl() {
	cd `ls -dt * | head -1`
}

cd_func ()
{
  local x2 the_new_dir adir index
  local -i cnt

  if [[ $1 ==  "--" ]]; then
    dirs -v
    return 0
  fi

  the_new_dir=$1
  [[ -z $1 ]] && the_new_dir=$HOME

  if [[ ${the_new_dir:0:1} == '-' ]]; then
    #
    # Extract dir N from dirs
    index=${the_new_dir:1}
    [[ -z $index ]] && index=1
    adir=$(dirs +$index)
    [[ -z $adir ]] && return 1
    the_new_dir=$adir
  fi

  #
  # '~' has to be substituted by ${HOME}
  [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"

  #
  # Now change to the new dir and add to the top of the stack
  pushd "${the_new_dir}" > /dev/null
  [[ $? -ne 0 ]] && return 1
  the_new_dir=$(pwd)

  #
  # Trim down everything beyond 11th entry
  popd -n +11 2>/dev/null 1>/dev/null

  #
  # Remove any other occurence of this dir, skipping the top of the stack
  for ((cnt=1; cnt <= 10; cnt++)); do
    x2=$(dirs +${cnt} 2>/dev/null)
    [[ $? -ne 0 ]] && return 0
    [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
    if [[ "${x2}" == "${the_new_dir}" ]]; then
      popd -n +$cnt 2>/dev/null 1>/dev/null
      cnt=cnt-1
    fi
  done

  return 0
}

alias cd=cd_func

if [[ $BASH_VERSION > "2.05a" ]]; then
  # ctrl+w shows the menu
  bind -x "\"\C-w\":cd_func -- ;"
fi

# added by Anaconda
env_anaconda(){
    export PATH=/home/kuba/anaconda2/bin:$PATH
    # export LD_LIBRARY_PATH=/home/kuba/anaconda2/lib:/home/kuba/anaconda2/lib64:/home/kuba/anaconda2/x86_64-conda_cos6-linux-gnu/sysroot/lib:$LD_LIBRARY_PATH
}