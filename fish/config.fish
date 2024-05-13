switch (uname)
    case Darwin
        set -x PATH /opt/homebrew/opt/grep/libexec/gnubin /usr/local/opt/coreutils/libexec/gnubin /Users/rlridenour/bin /opt/homebrew/bin /usr/local/bin /usr/local/sbin /usr/bin /bin /usr/sbin /sbin /Users/rlridenour/context/tex/texmf-osx-64/bin /Library/TeX/texbin /Applications/Visual\ Studio\ Code.app/Contents/Resources/app/bin /Users/rlridenour/.emacs.d/bin /Users/rlridenour/.doom-emacs/bin /Users/rlridenour/node_modules/stylelint/bin /opt/homebrew/opt/openjdk/bin /Users/rlridenour/node_modules/.bin /Users/rlridenour/.qlot/bin /Users/rlridenour/common-lisp/lem /Users/rlridenour/Library/Python/3.11/bin
    case Linux
        set -x PATH /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/games /usr/local/games /home/rlridenour/bin /snap/bin
end

# /Users/rlridenour/anaconda/bin  removed from Darwin path for homebrew
# /usr/local/sbin /usr/texbin

set -Ux EDITOR emacsclient -c

# Set up fzf key bindings
fzf --fish | source

set normal (set_color normal)
set magenta (set_color magenta)
set yellow (set_color yellow)
set green (set_color green)
set red (set_color red)
set gray (set_color -o black)

# Fish git prompt
set __fish_git_prompt_showdirtystate yes
set __fish_git_prompt_showstashstate yes
set __fish_git_prompt_showuntrackedfiles yes
set __fish_git_prompt_showupstream yes
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

# Status Chars
set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind -

function fish_prompt
    set last_status $status

    set_color $fish_color_cwd
    printf '%s' (hostname -s)
    printf /
    printf '%s' (prompt_pwd)
    set_color normal
    printf '%s ' (__fish_git_prompt)

    set_color normal
end

set fish_greeting
test -e {$HOME}/.iterm2_shell_integration.fish; and source {$HOME}/.iterm2_shell_integration.fish
set -g fish_user_paths "/usr/local/opt/node@10/bin" $fish_user_paths
set -g fish_user_paths "/usr/local/opt/node@8/bin" $fish_user_paths
set -gx HOMEBREW_GITHUB_API_TOKEN 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

