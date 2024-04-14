#![feature(exit_status_error)]
use std::env::{self, args, current_dir};
use std::error::Error;
use std::ffi::OsStr;
use std::fs::{self, remove_file};
use std::path::{Path, PathBuf};
use std::process::Command;

type Result<T = (), E = Box<dyn Error>> = std::result::Result<T, E>;

fn main() -> Result {
    match args().nth(1).as_deref() {
        Some("test") => test(),
        Some(cmd) => panic!("invalid command: {cmd}"),
        None => panic!("no subcommand given"),
    }
}

fn test() -> Result {
    let cargo = cargo();
    Command::new(&cargo)
        .arg("build")
        .arg("--release")
        .arg("-p")
        .arg("uwuc")
        .status()?
        .exit_ok()?;

    let uwuc = current_dir()?.join("target/release/uwuc");

    assert!(uwuc.exists());

    for file in walkdir::WalkDir::new("uitests") {
        if Path::new("./out").exists() {
            remove_file("./out")?;
        }
        let file = file?;
        if !file.file_type().is_file() {
            continue;
        }
        let path = file.path();
        if path.components().any(|x| x.as_os_str() == "auxiliary") {
            continue;
        }
        if path.extension().and_then(OsStr::to_str) != Some("uwu") {
            continue;
        }
        println!("{path:?}");
        let mut run = false;
        let mode = (|| -> Result<_> {
            let file = fs::read_to_string(path)?;
            if let Some(line) = file.lines().next() {
                if let Some(dir) = line.trim().strip_prefix("//") {
                    match dir.trim() {
                        "print-ast" => return Ok("print-ast"),
                        "print-mir" => return Ok("print-mir"),
                        "run" => {
                            run = true;
                            return Ok("gen");
                        }
                        _ => {}
                    }
                }
            }
            Ok("gen")
        })()?;
        let dir = tempfile::tempdir()?;
        let mut cmd = Command::new(&uwuc);
        cmd.args(["--use-ascii", "--dont-print-path"]);
        if run {
            cmd.arg(path.canonicalize()?);
        } else {
            cmd.arg(path);
        }
        cmd.args(["-m", mode]);
        if run {
            cmd.current_dir(&dir);
        }
        println!("{:?}", dir.path());

        let output = cmd.output()?;
        let output = String::from_utf8_lossy(&output.stderr);
        let output = output.trim();
        // println!("{output}");

        if !output.is_empty() {
            let new_path = path.with_file_name(format!(
                "{}.stderr",
                path.file_name().unwrap().to_string_lossy()
            ));
            if !new_path.exists() {
                panic!(
                    "{path:?} had stderr when its stderr file does not exist!\n\nstderr:\n{output}"
                );
            }
            let expected = fs::read_to_string(&new_path)?;
            let expected = expected.trim();
            if expected != output {
                let p = diffy::create_patch(expected, output);
                eprintln!("{p}\n");
                eprintln!("found: {output}");
                panic!();
            }
        }
        if run && Path::new("./out").exists() {
            let output = Command::new("./out").output()?;
            output.status.exit_ok()?;
            if !output.stdout.is_empty() {
                let output_str = String::from_utf8_lossy(&output.stdout);
                let new_path = path.with_file_name(format!(
                    "{}.stdout",
                    path.file_name().unwrap().to_string_lossy()
                ));
                if !new_path.exists() {
                    panic!("{path:?} had stdout when its stdout file does not exist!\n\nstdout:\n{output_str}");
                }
                let expected = fs::read_to_string(&new_path)?;
                assert_eq!(
                    fs::read_to_string(&new_path)?.trim(),
                    output_str.trim(),
                    "expected stdout to be equal:\n\nexpected:\n{expected}\n\nfound:\n{output_str}"
                );
            }
        }

        print!(".");
    }

    Ok(())
}

fn cargo() -> PathBuf {
    env::var("CARGO")
        .as_deref()
        .map_or_else(|_| Path::new("cargo"), Path::new)
        .into()
}
