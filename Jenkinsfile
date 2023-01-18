
def createDockerAndPush(version, packageVersion) {
  sh('cd backend')
  sh('make build -e packageVersion=0.0.2')
}

pipeline {
  agent {
    label 'sdk'
  }
  stages {
    stage('Checkout') {
      steps {
        scmSkip(deleteBuild: false, skipPattern:'.*\\[skip ci\\].*')
        script {
          if (sh(script: "git log -1 --pretty=%B | grep -F -ie '[skip ci]' -e '[ci skip]'", returnStatus: true) == 0) {
            currentBuild.result = 'ABORTED'
            error 'Aborting because commit message contains [skip ci]'
          }
        }
      }
    }

    stage('Build Image') {
      steps {
        createDockerAndPush('prod', '0.0.2')
      }
    }

    stage('Summary') {
      steps {
        script {
          echo 'Successfully compiled'
        }
      }
    }
  }
}
