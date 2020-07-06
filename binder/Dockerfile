# bash, sql, nosql and hadoop sessions
FROM gesiscss/binder-serhatcevikel-2dbounds-5f2019-5fsupp-107a8c:0ff6ba2e5e4622375f0ca47a1ae5559bb8e5f680
LABEL maintainer="serhatcevikel@yahoo.com"

# Make sure the contents of our repo are in ${HOME}
COPY --chown=jovyan:jovyan . ${HOME}

USER ${NB_USER}

RUN \
    #echo "[[ \$TERM != \"screen\" ]] && exec screen -q" >> ${HOME}/.bashrc;\
    rm -r ${HOME}/binder;

# Specify the default command to run
WORKDIR ${HOME}
