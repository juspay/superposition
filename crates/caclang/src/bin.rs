use std::fs;

use caclang::{ContextAwareConfig, DataType};
use clap::Parser;
use evalexpr::{ContextWithMutableVariables, HashMapContext};
use inquire::{CustomType, Text};

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    #[arg(short, long)]
    file: String,
}

pub fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let config = fs::read_to_string(args.file)?;
    let cac = ContextAwareConfig::parse(config)?;
    loop {
        let mut context = HashMapContext::new();
        for (dimension, props) in cac.dimensions.iter() {
            if props.data_type == DataType::String {
                if let Some(user_value) =
                    Text::new(format!("Value for {dimension}, hit Esc key to skip").as_str())
                        .prompt_skippable()?
                {
                    context.set_value(dimension.clone(), user_value.into())?;
                }
            } else if props.data_type == DataType::Number {
                if let Some(user_value) =
                    CustomType::<i64>::new(format!("Value for {dimension}, hit Esc key to skip").as_str())
                        .prompt_skippable()?
                {
                    context.set_value(dimension.clone(), user_value.into())?;
                }
            } else {
                if let Some(user_value) =
                    CustomType::<bool>::new(format!("Value for {dimension}, hit Esc key to skip").as_str())
                        .prompt_skippable()?
                {
                    context.set_value(dimension.clone(), user_value.into())?;
                }
            };
        }
        for ctx in cac.contexts.iter() {
            let eval = ctx.expr.eval_with_context(&context);
            if eval.is_ok() && eval?.as_boolean()? {
                println!("============================================");
                println!("context {}", ctx.context);
                println!("Expression {:?}", ctx.expr.to_string());
                println!("overrides {:?}", ctx.overrides);
                println!("priority {:?}", ctx.calculated_priority);
                println!("============================================");
            }
        }
        if CustomType::<bool>::new("Hit Esc to stop, enter to continue")
            .prompt_skippable()?
            .is_none()
        {
            return Ok(());
        }
    }
}
