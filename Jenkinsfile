node {
     git url: 'git@github.com:tdrhq/lal-elisp-1.git'

     try {
          sh "/home/arnold/builds/emacs/src/emacs --script test.el"
     } catch (any) {
          currentBuild.result = 'FAILURE'
     } finally {
          step([$class: 'Mailer', notifyEveryUnstableBuild: true, recipients: 'arnstein87@gmail.com', sendToIndividuals: true])
     }

     properties([
        pipelineTriggers([
            [$class: "GitHubPushTrigger"],
        ])
     ])
}
