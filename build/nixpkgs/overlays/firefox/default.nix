final: prev:

{
    firefox = prev.firefox.override {
        nativeMessagingHosts = [ prev.tridactyl-native ];
    };
}
