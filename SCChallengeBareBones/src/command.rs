use clap::{Arg, ArgMatches, Command};

// Creates a command template using clap (Command Line Argument Parser) and returns the arguments passed from the command line
pub fn command() -> ArgMatches {
    Command::new("bbones")
        .arg(
            Arg::new("file")
                .required(true)
                .help("Path to the bare bones input file"),
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .long("debug")
                .help("Enable debug mode")
                .action(clap::ArgAction::SetTrue),
        )
        .get_matches()
}
