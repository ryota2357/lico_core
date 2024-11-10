use syntax::{token::TokenStream, GreenNode, SyntaxError};

mod grammar;

mod parser;
pub(crate) use parser::Parser;

mod token_set;
pub(crate) use token_set::TokenSet;

mod tree_builder;
use tree_builder::TreeBuilder;

pub fn parse<'s>(token_stream: impl TokenStream<'s>) -> (GreenNode, Vec<SyntaxError>) {
    let (mut tree_builder, syntax_kinds) = TreeBuilder::from_token_stream(token_stream);
    let mut parser = Parser::new(syntax_kinds);
    grammar::source_file(&mut parser);
    let mut output = parser.finish();

    let mut forward_parents = Vec::new();
    for i in 0..output.event_count() {
        match output.take_event(i) {
            parser::Event::StartNode { kind, forward_parent } => {
                if forward_parent.is_none() {
                    tree_builder.start_node(kind);
                    continue;
                }
                forward_parents.push(kind);
                let mut idx = i;
                let mut fp = forward_parent;
                while let Some(fpi) = fp {
                    idx += fpi.get() as usize;
                    fp = match output.take_event(idx) {
                        parser::Event::StartNode { kind, forward_parent } => {
                            forward_parents.push(kind);
                            forward_parent
                        }
                        _ => unreachable!(),
                    };
                }
                for kind in forward_parents.drain(..).rev() {
                    tree_builder.start_node(kind);
                }
            }
            parser::Event::FinishNode => tree_builder.finish_node(),
            parser::Event::Token { kind } => tree_builder.push_token(kind),
            parser::Event::None => {}
            parser::Event::EmptyError { message } => tree_builder.push_error(message),
            parser::Event::StartError => tree_builder.start_error(),
            parser::Event::FinishError { message } => tree_builder.finish_error(message),
        }
    }
    tree_builder.finish()
}
