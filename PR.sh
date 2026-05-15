# git reset HEAD^

# Step 1: Checking Repository Status
# Display the current status of the repository
git status

# Step 2: Preparing Changes for Commit to a New Branch
# If the branch already exists
if git rev-parse --verify my-feature-branch; then
  # Switch to the existing branch
  git checkout my-feature-branch
  # Pull the latest changes from the upstream branch to avoid conflicts
  git pull origin main
else
  # Create and switch to a new branch 'my-feature-branch'
  git checkout -b my-feature-branch
  # Set the upstream branch for 'my-feature-branch' to track 'origin/main'
  git branch --set-upstream-to=origin/main my-feature-branch
  # Pull the latest changes from the 'main' branch
  git pull origin main
  # Push the new branch to the remote repository
  git push -u origin my-feature-branch
fi

# Step 3: Merging Changes and Preparing for Commit
# Fetch the latest changes from the main branch
git fetch origin
git merge origin/main

# crash avoidance
# git stash?
# git stash
# git pull --ff-only
# git stash pop

# Resolve conflicts if any, and then continue
# git mergetool (optional to open merge tool)
# git commit -m "Resolve merge conflicts" (if conflicts were resolved manually)

# Stage all changes for commit
git add .
# Commit the changes with a message
git commit -m "new push"

# Step 4: Pushing Changes to Remote Repository
# Rebase the current branch to apply commits on top of the latest changes
git pull origin my-feature-branch --rebase  #skip

# Push the changes in 'my-feature-branch' to the remote repository
git push origin my-feature-branch

# Step 5: Switching Back to Main Branch and Pulling Merged Content
# Switch to the 'main' branch
git checkout main

# GO to repo on GitHub and create a pull request
# Merge the pull request on GitHub


# Pull the merged content from the remote 'main' branch
git pull origin main

# delete local branch
git branch -d my-feature-branch

# force delete
# git branch -D my-feature-branch

# git fetch --all
# git branch -vv
