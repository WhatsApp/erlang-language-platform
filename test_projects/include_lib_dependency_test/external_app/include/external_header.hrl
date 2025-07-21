%% Header file that should not be accessible to main_app
%% since external_app is not a dependency of main_app

-define(EXTERNAL_MACRO, "This should not be accessible").
