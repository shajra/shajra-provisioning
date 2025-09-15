let

  wg-cake = cmd: ''
    for iface in \
            ( netstat -rn \
            | grep '192\.168\.7\..\+utun' \
            | tail -1 \
            | awk '{print $NF}'
            )
        sudo route -nv ${cmd} -host cake.home.arpa -interface $iface
    end
  '';

in
{
  functions = {
    wg-cake-add = {
      description = "Add Cake route for Wireguard interface";
      body = wg-cake "add";
    };
    wg-cake-delete = {
      description = "Remove Cake route for Wireguard interface";
      body = wg-cake "delete";
    };
  };
}
