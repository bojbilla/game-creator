## Contributing Guidelines

### Important

- You can `git push --force` to your own branch as long as you're the only one working on it.  
- **Never `git push --force` to `master`.**

#### Note

Please make sure you have set up Git to always rebase pulled changes instead of merging them.
To this end, just run:

    $ git config branch.autosetuprebase always

inside your working copy.

### Example

1. Either clone the repository if you haven't already.

        $ git clone https://github.com/reminisceme/game-creator.git

    or pull the latest changes with

        $ git pull origin

2. Checkout the `master` branch

        $ git checkout master

3. Create and switch to a new branch.

        $ git checkout -b feature/facebook-login

4. Implement the feature, and commit it.  
   Look into [`git commit --amend`](https://www.atlassian.com/git/tutorials/rewriting-history/git-commit--amend) and [`git rebase -i`](https://www.atlassian.com/git/tutorials/rewriting-history/git-rebase-i) to clean up your history, if needed.

5. Pull the latest `master` branch.

        $ git pull origin master

6. [Rebase](https://www.atlassian.com/git/tutorials/rewriting-history/git-rebase) your branch on the `master` branch, and fix the conflicts, if any.  
    This will ensure that your pull request can be easily merged into `master`.

        $ git rebase master

7. Push your branch to GitHub.

        $ git push origin feature/facebook-login

7. Go to [GitHub](https://github.com/reminisceme/game-creator), select your branch in the dropdown, and create a pull request.
