# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # All Vagrant configuration is done here. The most common configuration
  # options are documented and commented below. For a complete reference,
  # please see the online documentation at vagrantup.com.

  # Every Vagrant virtual environment requires a box to build off of.
  config.vm.box = 'ubuntu/trusty64'
  config.vm.provider 'virtualbox' do |v|
    v.memory = 1024
  end

  script = <<SCRIPT
echo "Starting"
set -e
set -x
export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y \
    -o Dpkg::Options::="--force-confdef" \
    -o Dpkg::Options::="--force-confold" \
    haskell-platform cabal-install

cd /vagrant

cabal update
cabal install

set +x
graph=./dist/build/check-graph/check-graph
if [ -f $graph ] ; then
    echo "======================================================"
    echo "check-graph is in ${graph}"
else
    echo "check-graph was not built correctly (it should be in ${graph})"
fi

SCRIPT

  config.vm.provision 'shell', inline: script
end
