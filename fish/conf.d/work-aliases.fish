### Work Aliases ###
#
# This file is for aliases that I use at work, things that I probably
# don't want checked into git.

#### PillPack
set INFRA_REPO '/Users/mbluemer/code/github.com/pillpack/infrastructure'
alias stagingsshid='pushd $INFRA_REPO; source venv/bin/activate.fish; ./scripts/wrap_mfa.py --aws-account-name BroadwingStaging --aws-role-name StagingAdministrator aws ssm start-session --region us-east-1 --target '
function stagingcmd
  pushd $INFRA_REPO
  source venv/bin/activate.fish
  ./scripts/wrap_mfa.py --aws-account-name BroadwingStaging --aws-role-name StagingAdministrator $argv
  deactivate
  popd
end

alias core='cd ~/code/github.com/pillpack/pillpack'
alias infra='cd ~/code/github.com/pillpack/infrastructure'
