# A range of git shortcuts

function g -d 'git'
  git $argv
end

function ga -d 'git add'
  g add $argv
end

function gb -d 'git branch'
  g branch $argv
end

function gl -d 'git pull'
  g pull $argv
end

function gp -d 'git push'
  g push $argv
end

function gst -d 'git status'
  g status
end
