use derive_syn_parse::Parse;
use syn::punctuated::Punctuated;

fn parse_terminated_default<T: syn::parse::Parse, P: syn::parse::Parse>(
    input: syn::parse::ParseStream,
) -> syn::Result<Punctuated<T, P>> {
    <Punctuated<T, P>>::parse_terminated(input)
}

fn parse_terminated_nonempty<T: syn::parse::Parse, P: syn::parse::Parse>(
    input: syn::parse::ParseStream,
) -> syn::Result<Punctuated<T, P>> {
    if input.is_empty() {
        T::parse(input)?;
        panic!("T parsed an empty ParseStream");
    }
    <Punctuated<T, P>>::parse_terminated(input)
}

mod kw {
    syn::custom_keyword!(root);
    syn::custom_keyword!(var);
    syn::custom_keyword!(term);
    syn::custom_keyword!(subset);
    syn::custom_keyword!(opaque);
    syn::custom_keyword!(guarding);
    syn::custom_keyword!(normalize);
    syn::custom_keyword!(pseudo);
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct Statement {
    #[call(syn::Attribute::parse_outer)]
    pub attrs: Vec<syn::Attribute>,
    pub body: StatementBody,
}

impl Statement {
    pub fn parse_many(input: syn::parse::ParseStream) -> syn::Result<Vec<Self>> {
        let mut parsed = vec![];
        while !input.is_empty() {
            parsed.push(input.parse()?);
        }
        Ok(parsed)
    }
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct Statements {
    #[call(Statement::parse_many)]
    pub statements: Vec<Statement>,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub enum StatementBody {
    #[peek(kw::root, name = "root keyword")]
    Root(RootStatement),
    #[peek(kw::var, name = "var keyword")]
    Var(VarStatement),
    #[peek(kw::term, name = "term keyword")]
    Term(TermStatement),
    #[peek(kw::subset, name = "subset keyword")]
    Subset(SubsetStatement),
    #[peek(syn::token::Brace, name = "brace delimited block of statements")]
    Group(GroupStatement),
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct RootStatement {
    pub root: kw::root,
    pub ident: syn::Ident,
    pub semi: syn::Token![;],
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct VarStatement {
    pub var: kw::var,
    pub ty: syn::Type,
    #[peek(syn::Token![:])]
    pub lit: Option<VarStatementLit>,
    pub semi: syn::Token![;],
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct VarStatementLit {
    pub colon: syn::Token![:],
    pub ty: syn::Type,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct TermStatement {
    pub term: kw::term,
    pub ident: syn::Ident,
    pub body: TermStatementBody,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub enum TermStatementBody {
    #[peek(syn::Token![=], name = "=")]
    Inline(TermStatementInline),
    #[peek(syn::Token![;], name = ";")]
    Unit(syn::Token![;]),
    #[peek(syn::token::Paren, name = "(")]
    Tuple(TermStatementTuple),
    #[peek(syn::token::Brace, name = "{")]
    Struct(TermStatementStruct),
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct TermStatementInline {
    pub equals: syn::Token![=],
    #[call(syn::Attribute::parse_outer)]
    pub attrs: Vec<syn::Attribute>,
    pub ty: syn::Type,
    pub semi: syn::Token![;],
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct TermStatementTuple {
    #[paren]
    pub paren: syn::token::Paren,
    #[inside(paren)]
    #[call(parse_terminated_default)]
    pub fields: Punctuated<TermStatementTupleField, syn::Token![,]>,
    pub semi: syn::Token![;],
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct TermStatementTupleField {
    #[call(syn::Attribute::parse_outer)]
    pub attrs: Vec<syn::Attribute>,
    pub ty: syn::Type,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct TermStatementStruct {
    #[brace]
    pub brace: syn::token::Brace,
    #[inside(brace)]
    #[call(parse_terminated_default)]
    pub fields: Punctuated<TermStatementStructField, syn::Token![,]>,
    #[peek(syn::Token![;])]
    pub semi: Option<syn::Token![;]>,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct TermStatementStructField {
    #[call(syn::Attribute::parse_outer)]
    pub attrs: Vec<syn::Attribute>,
    pub ident: syn::Ident,
    pub colon: syn::Token![:],
    pub ty: syn::Type,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct SubsetStatement {
    pub subset: kw::subset,
    pub ident: syn::Ident,
    #[paren]
    pub paren: syn::token::Paren,
    #[inside(paren)]
    #[call(parse_terminated_default)]
    pub items: Punctuated<SubsetItem, syn::Token![,]>,
    pub semi: Option<syn::Token![;]>,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct SubsetItem {
    #[peek(syn::Token![..])]
    pub dot_dot: Option<syn::Token![..]>,
    pub ident: syn::Ident,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct GroupStatement {
    #[brace]
    pub brace: syn::token::Brace,
    #[inside(brace)]
    #[call(Statement::parse_many)]
    pub statements: Vec<Statement>,
    #[peek(syn::Token![;])]
    pub semi: Option<syn::Token![;]>,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]

pub enum TermAttribute {
    #[peek(kw::normalize, name = "'normalize' keyword")]
    Normalize(kw::normalize),
    #[peek(kw::pseudo, name = "'pseudo' keyword")]
    Pseudo(TermAttributePseudo),
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]
pub struct TermAttributePseudo {
    pub pseudo: kw::pseudo,
    #[paren]
    pub paren: syn::token::Paren,
    #[inside(paren)]
    #[call(parse_terminated_nonempty)]
    pub types: Punctuated<syn::Ident, syn::Token![,]>,
}

#[allow(dead_code)] // fields only used for parsing
#[derive(Parse, Debug)]

pub enum FieldAttribute {
    #[peek(kw::opaque, name = "opaque keyword")]
    Opaque(kw::opaque),
    #[peek(kw::guarding, name = "guarding keyword")]
    Guarding(kw::guarding),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        let parsed = syn::parse_str::<Statements>(
            r###"

                    root Bitlevel;
                    var Var : Lit;

                    term Input = #[opaque] InputId;
                    term SteadyInput = #[opaque] SteadyInputId;
                    term And(UnorderedPair<Lit>);

                    #[normalize]
                    #[negatable]
                    {
                        term Xor(UnorderedPair<Lit>);

                        term Reg { #[guarding] next: Lit, init: Lit }

                        term Mux { select: Lit, choices: [Lit; 2] }

                        term Maj(UnorderedTuple<Lit, 3>);

                        term Lut3(Lut3);
                        term Lut4(Lut4);
                        term Lut5(Lut5);
                        term Lut6(Lut6);
                        term Lut7(Lut7);
                        term Lut8(Lut8);
                    }

                    subset LutK(Lut3, Lut4, Lut5, Lut6, Lut7, Lut8);

                    subset BitlevelCombTerm(And, Xor, ..LutK);
                    subset AigTerm(And, Reg);
                "###,
        );

        println!("{:#?}", parsed);
    }
}
