
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

source ~/git-completions.bash

export GOPATH=$HOME/go
export GOBIN="$HOME/go/gobin"
export PATH=$GOBIN:$PATH
export PATH=$PATH:$HOME/go/src/github.com/cosmos72/gomacro
export PATH=$HOME/bin:$PATH


env-houdini(){
  # apt-get install -y '^libxcb.*-dev' libx11-xcb-dev libglu1-mesa-dev libxrender-dev libxi-dev libxkbcommon-dev libxkbcommon-x11-dev
  export HOUDINI_USE_HFS_OCL=0
  pushd /opt/hfs19.0
  source ./houdini_setup
  popd
}

env_gcc650 (){
   export CC=$HOME/toolchains/gcc650/bin/gcc;
   export CPP=$HOME/toolchains/gcc650/bin/cpp;
   export CXX=$HOME/toolchains/gcc650/bin/g++;
   export CCC=$HOME/toolchains/gcc650/bin/gcc;
   export LD=$HOME/toolchains/gcc650/bin/gcc;

   alias gcc=$HOME/toolchains/gcc650/bin/gcc;
   alias cpp=$HOME/toolchains/gcc650/bin/cpp;
   alias g++=$HOME/toolchains/gcc650/bin/g++;
   alias ld=$HOME/toolchains/gcc650/bin/gcc;
   alias cc=$HOME/toolchains/gcc650/bin/gcc;

   export LD_LIBRARY_PATH=$HOME/toolchains/gcc650/lib64:$HOME/toolchains/gcc650/lib
}

env_qt512 (){
  export LD_LIBRARY_PATH=$HOME/toolchains/qt512/lib:$LD_LIBRARY_PATH
  export PATH=$HOME/toolchains/qt512/bin:$PATH
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

#if [[ $BASH_VERSION > "2.05a" ]]; #then
#  # ctrl+w shows the menu
#  bind -x "\"\C-w\":cd_func -- ;"
#fi


function env_gcc930(){
    export CC=$HOME/toolchains/gcc930/bin/gcc
    export CXX=$HOME/toolchains/gcc930/bin/g++
}

###############################################################################
# GIT
###############################################################################
# alias g-l='git log --pretty=oneline --abbrev-commit'
alias g-b='git branch -vv'
# alias g-c='git checkout'
# alias g-s='git status'
alias g-d='git diff --name-only'
# alias g-r='git for-each-ref --sort=committerdate refs/heads/'
alias g-prev='git reset --hard `git log -n 1 --skip 1 --format="%H"`'

g-r () {
  local path branch wt
  local -A wts
  if _g_w_in_linked 2>/dev/null; then
    g-wr
    return
  fi
  while IFS= read -r line || [ -n "$line" ]; do
    case "$line" in
      worktree\ *) path=${line#worktree } ;;
      branch\ *)
        branch=${line#branch }
        branch=${branch#refs/heads/}
        wts["$branch"]=$(basename "$path")
        ;;
      "") path=; branch= ;;
    esac
  done < <(git worktree list --porcelain)

  while IFS= read -r branch; do
    wt=${wts[$branch]}
    if [ -n "$wt" ]; then
      printf '%s  [worktree: %s]\n' "$branch" "$wt"
    else
      printf '%s\n' "$branch"
    fi
  done < <(git for-each-ref --sort=committerdate --format='%(refname:short)' refs/heads/)
}

g-l() {
  # Default to 5 commits if no count is provided
  local count=${1:-5}
  # Default to current branch (HEAD) if no branch is provided
  local branch=${2:-HEAD}

  echo "Showing $count commits from $branch"
  git log $branch --abbrev-commit -n "$count" --pretty=format:"%h %an %s"
  echo "" # Add a newline for cleanliness
}

# Switch branch in the main checkout only. A .worktrees directory stays on
# its branch; use g-wc to cd into another worktree instead.
g-c () {
  local rc wt find_rc
  if _g_w_in_linked 2>/dev/null; then
    echo "g-c cannot change branch inside a worktree (on $(git branch --show-current))" >&2
    if [ $# -eq 1 ] && [ "${1#-}" = "$1" ]; then
      echo "use g-wc $1 to move to another worktree" >&2
    else
      echo "use g-wc <branch> to move to another worktree" >&2
    fi
    return 1
  fi
  git checkout "$@"
  rc=$?
  if [ $rc -eq 0 ]; then
    return 0
  fi
  if [ $# -eq 1 ] && [ "${1#-}" = "$1" ]; then
    wt=$(_g_w_find "$1")
    find_rc=$?
    if [ $find_rc -eq 0 ]; then
      echo "use g-wc $1 to move to that worktree" >&2
    fi
  fi
  return $rc
}

g-s(){
  git status
}

# Linked worktrees always live under <main>/.worktrees/.
_g_w_in_linked () {
  local root cwd
  root=$(_g_w_root) || return 1
  [ -d "$root" ] || return 1
  cwd=$(pwd -P)
  root=$(cd "$root" && pwd -P) || return 1
  case "$cwd" in
    "$root"|"$root"/*) return 0 ;;
    *) return 1 ;;
  esac
}

_g_w_root () {
  local main
  main=$(git worktree list --porcelain | { IFS= read -r line; printf '%s\n' "${line#worktree }"; })
  if [ -z "$main" ]; then
    return 1
  fi
  printf '%s\n' "$main/.worktrees"
}

_g_w_ensure_root () {
  local root exclude
  root=$(_g_w_root) || return 1
  mkdir -p "$root"
  exclude=$(git rev-parse --git-path info/exclude)
  if [ -n "$exclude" ] && ! grep -qxF '.worktrees/' "$exclude" 2>/dev/null; then
    printf '%s\n' '.worktrees/' >> "$exclude"
  fi
  printf '%s\n' "$root"
}

# Resolve a .worktrees entry by directory name, branch, path, or unique prefix.
# Prints the worktree path on stdout.
_g_w_find () {
  local query="$1"
  local root path branch base
  local -a exact prefix
  if [ -z "$query" ]; then
    echo "usage: g-wc|g-wd <worktree-name>" >&2
    return 1
  fi
  root=$(_g_w_root) || return 1
  while IFS= read -r line || [ -n "$line" ]; do
    case "$line" in
      worktree\ *) path=${line#worktree } ;;
      branch\ *)
        branch=${line#branch }
        branch=${branch#refs/heads/}
        ;;
      detached) branch="(detached)" ;;
      bare) branch="(bare)" ;;
      "")
        if [ -n "$path" ]; then
          case "$path" in
            "$root"/*)
              base=$(basename "$path")
              if [ "$base" = "$query" ] || [ "$branch" = "$query" ] || [ "$path" = "$query" ] || [ "$path" = "$root/${query//\//-}" ]; then
                exact+=("$path")
              elif [ "$base" = "${base#"$query"}" ] && [ "$branch" = "${branch#"$query"}" ]; then
                :
              else
                prefix+=("$path")
              fi
              ;;
          esac
        fi
        path=
        branch=
        ;;
    esac
  done < <(git worktree list --porcelain)

  local -a matches
  if [ ${#exact[@]} -gt 0 ]; then
    matches=("${exact[@]}")
  else
    matches=("${prefix[@]}")
  fi

  if [ ${#matches[@]} -eq 0 ]; then
    return 1
  fi
  if [ ${#matches[@]} -gt 1 ]; then
    echo "ambiguous worktree '$query':" >&2
    printf '  %s\n' "${matches[@]}" >&2
    return 2
  fi
  printf '%s\n' "${matches[0]}"
}

# Resolve a local branch by exact name or unique prefix. Prints the branch name.
_g_w_branch () {
  local query="$1"
  local b
  local -a prefixes
  if [ -z "$query" ]; then
    return 1
  fi
  while IFS= read -r b; do
    if [ "$b" = "$query" ]; then
      printf '%s\n' "$b"
      return 0
    fi
    if [ "$b" != "${b#"$query"}" ]; then
      prefixes+=("$b")
    fi
  done < <(git for-each-ref --format='%(refname:short)' refs/heads/)
  if [ ${#prefixes[@]} -eq 1 ]; then
    printf '%s\n' "${prefixes[0]}"
    return 0
  fi
  if [ ${#prefixes[@]} -gt 1 ]; then
    echo "ambiguous branch '$query':" >&2
    printf '  %s\n' "${prefixes[@]}" >&2
    return 2
  fi
  return 1
}

# List .worktrees branch names, least recently modified first (recent at the bottom).
g-wr () {
  local root path branch ts gitdir f mt
  local -a rows
  root=$(_g_w_root) || return 1
  while IFS= read -r line || [ -n "$line" ]; do
    case "$line" in
      worktree\ *) path=${line#worktree } ;;
      branch\ *)
        branch=${line#branch }
        branch=${branch#refs/heads/}
        ;;
      detached) branch="(detached)" ;;
      bare) branch="(bare)" ;;
      "")
        if [ -n "$path" ]; then
          case "$path" in
            "$root"/*)
              ts=$(find "$path" \( -name .git -o -name target \) -prune -o -printf '%T@\n' 2>/dev/null | awk 'BEGIN{m=0} $1>m{m=$1} END{printf "%.0f\n", m}')
              gitdir=$(git -C "$path" rev-parse --absolute-git-dir 2>/dev/null) || gitdir=
              for f in "$gitdir/HEAD" "$gitdir/index"; do
                [ -e "$f" ] || continue
                mt=$(stat -c %Y "$f" 2>/dev/null) || continue
                if [ "$mt" -gt "${ts:-0}" ]; then
                  ts=$mt
                fi
              done
              rows+=("${ts:-0}	${branch:--}")
              ;;
          esac
        fi
        path=
        branch=
        ;;
    esac
  done < <(git worktree list --porcelain)

  if [ ${#rows[@]} -eq 0 ]; then
    return
  fi
  printf '%s\n' "${rows[@]}" | sort -n | cut -f2-
}

# cd to a .worktrees checkout. If none exists, add one for an existing local branch.
# g-wc -b <branch>  creates a new branch + worktree under .worktrees/ from HEAD.
g-wc () {
  local path root name dir rc
  if [ "$1" = "-b" ]; then
    shift
    name=$1
    if [ -z "$name" ] || [ -n "$2" ]; then
      echo "usage: g-wc -b <branch>" >&2
      return 1
    fi
    if git show-ref --verify --quiet "refs/heads/$name"; then
      echo "branch '$name' already exists; use: g-wc $name" >&2
      return 1
    fi
    root=$(_g_w_ensure_root) || return 1
    dir=${name//\//-}
    path="$root/$dir"
    git worktree add -b "$name" "$path" || return 1
    cd "$path"
    return
  fi
  if [ -z "$1" ]; then
    echo "usage: g-wc <branch>" >&2
    echo "       g-wc -b <new-branch>" >&2
    return 1
  fi
  path=$(_g_w_find "$1")
  rc=$?
  if [ $rc -eq 0 ]; then
    cd "$path"
    return
  fi
  if [ $rc -eq 2 ]; then
    return 1
  fi
  name=$(_g_w_branch "$1")
  rc=$?
  if [ $rc -ne 0 ]; then
    if [ $rc -eq 1 ]; then
      echo "no worktree or branch matching '$1'" >&2
    fi
    return 1
  fi
  root=$(_g_w_ensure_root) || return 1
  dir=${name//\//-}
  path="$root/$dir"
  git worktree add "$path" "$name" || return 1
  cd "$path"
}

# Remove a .worktrees checkout by name. Pass -f/--force anywhere to force.
# If you are inside that worktree, cd to the main worktree first.
g-wd () {
  local name force path root main cwd abs arg rc
  force=()
  for arg in "$@"; do
    case "$arg" in
      -f|--force) force=(--force) ;;
      -*) echo "usage: g-wd [-f|--force] <worktree-name>" >&2; return 1 ;;
      *)
        if [ -n "$name" ]; then
          echo "usage: g-wd [-f|--force] <worktree-name>" >&2
          return 1
        fi
        name=$arg
        ;;
    esac
  done
  path=$(_g_w_find "$name")
  rc=$?
  if [ $rc -ne 0 ]; then
    if [ $rc -eq 1 ]; then
      echo "no worktree matching '$name' in .worktrees" >&2
    fi
    return 1
  fi
  root=$(_g_w_root) || return 1
  main=${root%/.worktrees}
  cwd=$(pwd -P)
  abs=$(cd "$path" && pwd -P)
  if [ "$cwd" = "$abs" ] || [ "${cwd#"$abs"/}" != "$cwd" ]; then
    cd "$main" || return 1
  fi
  git worktree remove "${force[@]}" "$path"
}
