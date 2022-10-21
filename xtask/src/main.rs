#![feature(exit_status_error)]
use std::env::{self, args, current_dir};
use std::error::Error;
use std::ffi::OsStr;
use std::fs;
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
        .arg("terryc")
        .status()?
        .exit_ok()?;

    let terryc = current_dir()?.join("target/release/terryc");

    assert!(terryc.exists());

    for file in walkdir::WalkDir::new("uitests") {
        let file = file?;
        if !file.file_type().is_file() {
            continue;
        }
        let path = file.path();
        if path.extension().and_then(OsStr::to_str) != Some("terry") {
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
                            return Ok("out-class");
                        }
                        _ => {}
                    }
                }
            }
            Ok("compile")
        })()?;
        let dir = tempfile::tempdir()?;
        let mut cmd = Command::new(&terryc);
        cmd.arg("--use-ascii");
        if run {
            cmd.arg(path.canonicalize()?);
        } else {
            cmd.arg(path);
        }
        cmd.arg(mode);
        if run {
            cmd.current_dir(&dir);
        }
        println!("{:?}", dir.path());

        let output = cmd.output()?;
        println!("{}", String::from_utf8_lossy(&output.stderr));

        if run {
            // assert!(dir.path().join("Main.class").exists());

            /*let disasm = Command::new("javap")
                    .current_dir(&dir)
                    .arg("-v")
                    .arg("-p")
                    .arg("-c")
                    .arg(dir.path().join("Main.class"))
                    .output()?;
                let bytes = disasm.stderr;
                println!("{}", String::from_utf8_lossy(&bytes));
                let output_str = String::from_utf8_lossy(&disasm.stdout);
                let new_path = path.with_file_name(format!(
                    "{}.disasm",
                    path.file_name().unwrap().to_string_lossy()
                ));
                if !new_path.exists() {
                    panic!("{path:?} disasm file does not exist!\n\ndisasm:\n{output_str}");
                }
                let expected = fs::read_to_string(&new_path)?;
                let expected = expected.trim();
                fn rmline(s: &str) -> &str {
                    s.trim_start_matches(|c| c != '\n').get(1..).unwrap_or(s)
                }
                let s = rmline(rmline(rmline(output_str.trim())));
                if expected != s {
                    let p = diffy::create_patch(expected, s);
                    eprintln!("{p}\n");
                    eprintln!("found: {s}");
                    panic!();
                }
                let output = Command::new("java")
                    .current_dir(&dir)
                    .arg("-noverify")
                    .arg("Main")
                    .output()?;
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
            if !output.stderr.is_empty() {
                let output_str = String::from_utf8_lossy(&output.stderr);
                let new_path = path.with_file_name(format!(
                    "{}.stderr",
                    path.file_name().unwrap().to_string_lossy()
                ));
                if !new_path.exists() {
                    panic!("{path:?} had stderr when its stderr file does not exist!\n\nstderr:\n{output_str}");
                }
                let expected = fs::read_to_string(&new_path)?;
                assert_eq!(
                    expected.trim(),
                    output_str.trim(),
                    "expected stderr to be equal ({path:?}):\n\nexpected:\n{expected}\n\nfound:\n{output_str}"
                );*/
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
