// Template Jenkinsfile for R CI/CD Tasks
// Author: Sebastian Warnholz
pipeline {
    agent none
    options { disableConcurrentBuilds() }
    environment {
        CUR_PROJ = 'ggcorpident' // github repo name
        CUR_PKG = 'ggCorpIdent' // r-package name
        CUR_PKG_FOLDER = '.' // defaults to root
    }
    stages {

        stage('Testing with R-3.5.1') {
            agent { label 'test' }
            when { not { branch 'depl' } }
            steps {
                sh '''
                docker build --pull -t tmp-$CUR_PROJ .
                docker run --rm --network host tmp-$CUR_PROJ check
                docker rmi tmp-$CUR_PROJ
                '''
            }
        }

        stage('Deploy R-package') {
            agent { label 'eh2' }
            when { branch 'master' }
            steps {
                sh '''
                rm -vf *.tar.gz
                docker build --pull -t tmp-$CUR_PROJ .
                docker run --rm --network host -v $PWD:/app --user `id -u`:`id -g` tmp-$CUR_PROJ R CMD build .
                docker rmi tmp-$CUR_PROJ
                R -e "drat::insertPackage('`echo $CUR_PKG`_`grep -E '^Version:[ \t]*[0-9.]{3,10}' $CUR_PKG_FOLDER/DESCRIPTION | awk '{print $2}'`.tar.gz', '/var/www/html/r-repo'); drat::archivePackages(repopath = '/var/www/html/r-repo')"
                '''
            }
        }

    }

}
