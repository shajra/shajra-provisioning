self: super:

{
    firefox = super.firefox.override {
        nativeMessagingHosts = [ super.tridactyl-native ];
    };
}
