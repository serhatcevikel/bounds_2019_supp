# bash, sql, nosql and hadoop sessions
FROM gesiscss/binder-serhatcevikel-2dbounds-5f2019-5fsupp-107a8c:0ff6ba2e5e4622375f0ca47a1ae5559bb8e5f680
LABEL maintainer="serhatcevikel@yahoo.com"

# create hadoop user
ENV NB_USER bounds
ENV NB_UID 1002
ENV HOME /home/${NB_USER}

USER root
RUN adduser --disabled-password \
    --gecos "Default user" \
    --uid ${NB_UID} \
    ${NB_USER} && \
    echo "bounds:bounds" | chpasswd && \
    usermod -aG sudo ${NB_USER} && \
    echo "bounds ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/bounds;

# Make sure the contents of our repo are in ${HOME}
COPY --chown=bounds:bounds . ${HOME}

USER ${NB_USER}

RUN \
    #echo "[[ \$TERM != \"screen\" ]] && exec screen -q" >> ${HOME}/.bashrc;\
    rm -r ${HOME}/binder;

# Specify the default command to run
WORKDIR ${HOME}
