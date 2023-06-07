def getRegistryHost(aws_acc_id, region) {
    return aws_acc_id + ".dkr.ecr." + region + ".amazonaws.com";
}

pipeline {
  agent { label 'hypersdk' }
  environment {
    SKIP_CI = false;
    REGION = "ap-south-1";
    REGISTRY_HOST_SBX = getRegistryHost("701342709052", REGION);
    REGISTRY_HOST_PROD = getRegistryHost("980691203742", REGION);
    AUTOPILOT_HOST_INTEG = "autopilot-eks2.internal.svc.k8s.integ.mum.juspay.net";
  }
  stages {
    stage('Checkout') {
      steps {
        script {
	  isSkipCI = sh(script: "git log -1 --pretty=%B | grep -F -ie '[skip ci]' -e '[ci skip]'",
	                returnStatus: true)
          if (isSkipCI == 0) {
	    env.SKIP_CI = true;
          }
          env.COMMIT_HASH = sh(returnStdout: true, script: "git rev-parse --short HEAD").trim()
        }
      }
    }

    stage('Test') {
      when { expression { SKIP_CI == 'false' } }
      steps { sh 'make ci-test' }
    }

    stage('Build Image') {
      when {
        expression { SKIP_CI == 'false' }
	branch 'main'
      }
      steps { sh 'make ci-build -e VERSION=${COMMIT_HASH}' }
    }

    stage('Push Image To Sandbox Registry') {
      when {
        expression { SKIP_CI == 'false' }
	branch 'main'
      }
      steps {
	sh '''make ci-push -e \
                VERSION=${COMMIT_HASH} \
                REGION=${REGION} \
                REGISTRY_HOST=${REGISTRY_HOST_SBX}
           '''
      }
    }

    // Disabled for now, as prod setup is not complete.
    //stage('Push Image To Production Registry') {
    //  when {
    //    expression { SKIP_CI == 'false' }
    //    branch 'main'
    //  }
    //  steps {
    //    sh '''make ci-push -e \
    //            VERSION=${COMMIT_HASH} \
    //            REGION=${REGION} \
    //            REGISTRY_HOST=${REGISTRY_HOST_PROD}
    //       '''
    //  }
    //}

    stage('Create Integ Release Tracker') {
      when {
        expression { SKIP_CI == 'false' }
	branch 'main'
      }
      environment {
        CREDS = credentials('AP_INTEG_ID')
        COMMIT_MSG = sh(returnStdout: true, script: "git log --format=format:%s -1")
        CHANGE_LOG = "Commit message: ${COMMIT_MSG}";
        AUTHOR_NAME = sh(returnStdout: true, script: "git log -1 --pretty=format:'%ae'")
      }
      steps {
        sh """curl -v --location --request POST 'https://${AUTOPILOT_HOST_INTEG}/release' \
                --header 'Content-Type: application/json' \
                --header 'Authorization: Basic ${CREDS_PSW}' \
                --data-raw '{
                      "service": ["CONTEXT_AWARE_CONFIG"],
                      "release_manager": "${AUTHOR_NAME}",
                      "release_tag": "",
                      "new_version": "${COMMIT_HASH}",
                      "docker_image" : "${COMMIT_HASH}",
                      "priority" : 0,
                      "cluster" : "INTEG_CLUSTER",
                      "change_log": "${CHANGE_LOG}",
                      "rollout_strategy": [
                          {
                              "rollout": 100,
                              "cooloff": 0,
                              "pods": 1
                          }
                      ],
                      "description": "${CHANGE_LOG}",
                      "product": "HYPER_SDK",
                      "mode" : "AUTO",
                      "env" : "INTEG"
                }';
           """
      }
    }

    stage('Summary') {
      steps {
        script {
          echo 'Build Success'
        }
      }
    }
  }
}
