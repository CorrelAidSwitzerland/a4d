require(RInno)
install_inno()

# Example app included with RInno package
example_app(app_dir = "app")

# Build an installer
create_app(app_name = "test", app_dir = "app")
compile_iss()