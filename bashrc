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

# source ~/git-completion.bash

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