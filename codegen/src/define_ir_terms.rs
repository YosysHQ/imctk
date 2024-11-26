use std::collections::{BTreeMap, BTreeSet};

use quote::{quote, ToTokens};

mod parser;

pub use parser::Statements;

#[derive(Default)]
struct PartialModel {
    root: Option<syn::Ident>,
    var_lit: Option<(syn::Type, syn::Type)>,
    terms: BTreeMap<syn::Ident, TermModel>,
    subsets: BTreeMap<syn::Ident, PartialSubsetModel>,
    attribute_stack: Vec<syn::Attribute>,
}

impl PartialModel {
    pub fn process_statement(&mut self, statement: parser::Statement) -> syn::Result<()> {
        let attribute_len = self.attribute_stack.len();
        self.attribute_stack.extend(statement.attrs);

        let res = self.process_statement_body(statement.body);

        self.attribute_stack.truncate(attribute_len);

        res
    }

    fn check_no_attributes(&self, statement: &str) -> syn::Result<()> {
        if let Some(attribute) = self.attribute_stack.first() {
            Err(syn::Error::new_spanned(
                attribute,
                format!("{statement} statement doesn't support attributes"),
            ))
        } else {
            Ok(())
        }
    }

    fn check_ident(&self, ident: &syn::Ident) -> syn::Result<()> {
        if let Some((prev, _)) = self.terms.get_key_value(ident) {
            return Err(syn::Error::new_spanned(
                quote![ #prev #ident ],
                format!("{ident} already defined as term"),
            ));
        }
        if let Some((prev, _)) = self.subsets.get_key_value(ident) {
            return Err(syn::Error::new_spanned(
                quote![ #prev #ident ],
                format!("{ident} already defined as subset"),
            ));
        }

        Ok(())
    }

    fn process_statement_body(&mut self, body: parser::StatementBody) -> syn::Result<()> {
        match body {
            parser::StatementBody::Root(root_statement) => {
                self.check_no_attributes("root")?;
                if self.root.is_some() {
                    return Err(syn::Error::new_spanned(
                        root_statement.root,
                        "root type already specified",
                    ));
                }
                self.root = Some(root_statement.ident);
            }
            parser::StatementBody::Var(var_statement) => {
                self.check_no_attributes("var")?;
                if self.var_lit.is_some() {
                    return Err(syn::Error::new_spanned(
                        var_statement.var,
                        "var type already specified",
                    ));
                }
                let var = var_statement.ty;
                let lit = if let Some(lit) = var_statement.lit {
                    lit.ty
                } else {
                    var.clone()
                };
                self.var_lit = Some((var, lit));
            }
            parser::StatementBody::Term(term_statement) => {
                self.check_ident(&term_statement.ident)?;
                let ident = term_statement.ident;
                let mut fields = vec![];
                let kind = match term_statement.body {
                    parser::TermStatementBody::Unit(_) => TermKind::Unit,
                    parser::TermStatementBody::Inline(term_statement_inline) => {
                        fields.push(TermFieldModel::new(
                            None,
                            term_statement_inline.ty,
                            term_statement_inline.attrs,
                        )?);
                        TermKind::Inline
                    }
                    parser::TermStatementBody::Tuple(term_statement_tuple) => {
                        for field in term_statement_tuple.fields {
                            fields.push(TermFieldModel::new(None, field.ty, field.attrs)?);
                        }
                        TermKind::Tuple
                    }
                    parser::TermStatementBody::Struct(term_statement_struct) => {
                        for field in term_statement_struct.fields {
                            fields.push(TermFieldModel::new(
                                Some(field.ident),
                                field.ty,
                                field.attrs,
                            )?);
                        }
                        TermKind::Struct
                    }
                };
                self.terms.insert(
                    ident.clone(),
                    TermModel::new(ident, kind, fields, self.attribute_stack.clone())?,
                );
            }
            parser::StatementBody::Subset(subset_statement) => {
                self.check_no_attributes("subset")?;
                self.check_ident(&subset_statement.ident)?;

                let mut terms = BTreeSet::default();
                let mut included = BTreeSet::default();
                for item in subset_statement.items {
                    if item.dot_dot.is_some() {
                        included.insert(item.ident);
                    } else {
                        terms.insert(item.ident);
                    }
                }

                self.subsets.insert(
                    subset_statement.ident.clone(),
                    PartialSubsetModel {
                        ident: subset_statement.ident,
                        terms,
                        included,
                    },
                );
            }
            parser::StatementBody::Group(group) => {
                for statement in group.statements {
                    self.process_statement(statement)?;
                }
            }
        }
        Ok(())
    }

    fn finish(mut self) -> syn::Result<Model> {
        let Some(root) = self.root.clone() else {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "missing 'root' statement",
            ));
        };
        let Some((var, lit)) = self.var_lit.clone() else {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "missing 'var' statement",
            ));
        };

        self.check_ident(&root)?;

        let root_subset = PartialSubsetModel {
            ident: root.clone(),
            terms: self
                .terms
                .values()
                .filter(|term| !term.is_pseudo)
                .map(|term| term.ident.clone())
                .collect(),
            included: Default::default(),
        };

        self.subsets.insert(root.clone(), root_subset);

        Ok(Model {
            root,
            var,
            lit,
            terms: self.terms,
            subsets: self
                .subsets
                .into_iter()
                .map(|(ident, subset)| (ident, subset.finish()))
                .collect(),
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum TermKind {
    Inline,
    Unit,
    Tuple,
    Struct,
}

#[derive(Debug)]
struct TermModel {
    ident: syn::Ident,
    kind: TermKind,
    has_normalize: bool,
    is_pseudo: bool,
    fields: Vec<TermFieldModel>,
}

impl TermModel {
    fn new(
        ident: syn::Ident,
        kind: TermKind,
        fields: Vec<TermFieldModel>,
        attributes: Vec<syn::Attribute>,
    ) -> syn::Result<Self> {
        if matches!(kind, TermKind::Inline) {
            assert_eq!(fields.len(), 1);
        }
        let mut new = Self {
            ident,
            kind,
            has_normalize: false,
            is_pseudo: false,
            fields,
        };

        new.process_attributes(attributes)?;
        Ok(new)
    }

    fn process_attributes(&mut self, attributes: Vec<syn::Attribute>) -> syn::Result<()> {
        for attr in attributes {
            let field_attr: parser::TermAttribute = syn::parse2(attr.meta.to_token_stream())?;
            match field_attr {
                parser::TermAttribute::Normalize(_) => self.has_normalize = true,
                parser::TermAttribute::Pseudo(_) => self.is_pseudo = true,
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct TermFieldModel {
    ident: Option<syn::Ident>,
    is_guarding: bool,
    is_opaque: bool,
    ty: syn::Type,
}

impl TermFieldModel {
    fn new(
        ident: Option<syn::Ident>,
        ty: syn::Type,
        attributes: Vec<syn::Attribute>,
    ) -> syn::Result<Self> {
        let mut new = Self {
            ident,
            is_guarding: false,
            is_opaque: false,
            ty,
        };
        new.process_attributes(attributes)?;
        Ok(new)
    }

    fn process_attributes(&mut self, attributes: Vec<syn::Attribute>) -> syn::Result<()> {
        for attr in attributes {
            let field_attr: parser::FieldAttribute = syn::parse2(attr.meta.to_token_stream())?;
            match field_attr {
                parser::FieldAttribute::Guarding(_guarding) => self.is_guarding = true,
                parser::FieldAttribute::Opaque(_opaque) => self.is_opaque = true,
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct SubsetModel {
    ident: syn::Ident,
    terms: BTreeSet<syn::Ident>,
}

struct PartialSubsetModel {
    ident: syn::Ident,
    terms: BTreeSet<syn::Ident>,
    included: BTreeSet<syn::Ident>,
}

impl PartialSubsetModel {
    fn finish(self) -> SubsetModel {
        assert!(self.included.is_empty());
        SubsetModel {
            ident: self.ident,
            terms: self.terms,
        }
    }
}

#[derive(Debug)]
pub struct Model {
    root: syn::Ident,
    var: syn::Type,
    lit: syn::Type,

    terms: BTreeMap<syn::Ident, TermModel>,
    subsets: BTreeMap<syn::Ident, SubsetModel>,
}

impl Model {
    pub fn new(statements: parser::Statements) -> syn::Result<Self> {
        let mut partial = PartialModel::default();

        for statement in statements.statements {
            partial.process_statement(statement)?;
        }

        partial.finish()
    }
}

fn suffixed(ident: &syn::Ident, suffix: &str) -> syn::Ident {
    syn::Ident::new(&format!("{ident}{suffix}"), ident.span())
}

impl ToTokens for Model {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let mut term_types = BTreeMap::default();
        for term in self.terms.values() {
            let TermModel {
                ref ident,
                kind,
                ref fields,
                ..
            } = *term;

            let term_type = if kind == TermKind::Inline {
                fields[0].ty.clone()
            } else {
                let mut field_tokens = proc_macro2::TokenStream::default();

                tokens.extend(quote![#[derive(PartialEq, Eq, Hash, Debug, Clone)]]);

                match kind {
                    TermKind::Unit => {
                        tokens.extend(quote![pub struct #ident;]);
                    }
                    TermKind::Tuple => {
                        for TermFieldModel { ident, ty, .. } in fields {
                            assert!(ident.is_none());
                            field_tokens.extend(quote![pub #ty,]);
                        }
                        tokens.extend(quote![pub struct #ident(#field_tokens);]);
                    }
                    TermKind::Struct => {
                        for TermFieldModel { ident, ty, .. } in fields {
                            let ident = ident.as_ref().unwrap();
                            field_tokens.extend(quote![pub #ident: #ty,]);
                        }
                        tokens.extend(quote![pub struct #ident { #field_tokens }]);
                    }
                    TermKind::Inline => unreachable!(),
                }

                syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: ident.clone().into(),
                })
            };
            term_types.insert(term.ident.clone(), term_type);
        }

        let var_type = &self.var;
        let lit_type = &self.lit;
        let node_sym = suffixed(&self.root, "Node");
        let node_mut_sym = suffixed(&self.root, "NodeMut");
        let catalog_sym = suffixed(&self.root, "Catalog");
        let root_variant_sym = suffixed(&self.root, "Variant");
        let root_term_sym = suffixed(&self.root, "Term");
        let root_term_mut_sym = suffixed(&self.root, "TermMut");

        let root_subset = &self.subsets[&self.root];

        let mut drop_item_tokens = proc_macro2::TokenStream::default();
        let mut item_layout_tokens = proc_macro2::TokenStream::default();

        for ident in root_subset.terms.iter() {
            let term_type = &term_types[ident];
            drop_item_tokens.extend(quote! {
                #root_variant_sym::#ident => item.cast::<#node_sym<#term_type>>().drop_in_place(),
            });
            item_layout_tokens.extend(quote! {
                #root_variant_sym::#ident => Layout::new::<#node_sym<#term_type>>(),
            });
        }

        tokens.extend(quote! {
            #[derive(Clone, Debug)]
            pub struct #node_sym<Term> {
                pub output: #lit_type,
                pub term: Term,
            }

            #[derive(Debug)]
            pub struct #node_mut_sym<'a, Term> {
                pub output: &'a mut #lit_type,
                pub term: Term,
            }
            impl<'a, Term> From<&'a mut #node_sym<Term>> for #node_mut_sym<'a, &'a mut Term> {
                fn from(value: &'a mut #node_sym<Term>) -> Self {
                    #node_mut_sym {
                        output: &mut value.output,
                        term: &mut value.term,
                    }
                }
            }

            #[derive(Default, Debug)]
            pub struct #catalog_sym;

            unsafe impl PagedStorageCatalog for #catalog_sym {
                type Variant = #root_variant_sym;

                unsafe fn drop_item(&mut self, variant: Self::Variant, item: *mut u8) {
                    unsafe {
                        match variant {
                            #drop_item_tokens
                        }
                    }
                }

                fn item_layout(&self, variant: Self::Variant) -> std::alloc::Layout {
                    match variant {
                        #item_layout_tokens
                    }
                }
            }

            impl IndexedCatalog for #catalog_sym {
                type Var = #var_type;
                type Lit = #lit_type;
                type NodeId = u32;
                type Node = #node_sym<#root_term_sym>;
                type NodeRef<'a> = #node_sym<#root_term_sym>;
                type NodeMut = #node_mut_sym<'static, #root_term_mut_sym<'static>>;
                type Term = #root_term_sym;
                type TermRef<'a> = #root_term_sym;

                fn term_eq(a: &Self::TermRef<'_>, b: &Self::TermRef<'_>) -> bool {
                    a == b
                }
            }

            impl<'a> IndexedNodeMutFamily<#catalog_sym> for #node_mut_sym<'a, #root_term_mut_sym<'a>> {
                type Instantiate<'b>
                    = #node_mut_sym<'b, #root_term_mut_sym<'b>>
                where
                    'a: 'b;
            }

            impl ContainedVars<#catalog_sym> for Lit {
                fn contained_vars_into_extend(
                    &self,
                    sink: &mut impl Extend<#var_type>,
                ) {
                    sink.extend([self.var()])
                }

                fn map_contained_vars(&self, mut fun: impl FnMut(#lit_type) -> #lit_type) -> Self {
                    fun(*self)
                }
            }

            impl ContainedVars<#catalog_sym> for UnorderedPair<Lit> {
                fn contained_vars_into_extend(
                    &self,
                    sink: &mut impl Extend<#var_type>,
                ) {
                    sink.extend((*self).map(Lit::var).into_iter())
                }

                fn map_contained_vars(&self, mut fun: impl FnMut(#lit_type) -> #lit_type) -> Self {
                    UnorderedPair::map(*self, fun)
                }
            }
        });

        for subset in self.subsets.values() {
            let SubsetModel { ident, terms } = subset;
            let variant_sym = suffixed(ident, "Variant");
            let mut variant_tokens = proc_macro2::TokenStream::default();
            for term in terms {
                variant_tokens.extend(quote![#term,]);
            }
            tokens.extend(quote! {
                #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, EnumIter)]
                pub enum #variant_sym {
                    #variant_tokens
                }
            });
            let mut term_tokens = proc_macro2::TokenStream::default();
            for ident in terms {
                let term_type = &term_types[ident];
                term_tokens.extend(quote![#ident(#term_type),]);
            }
            let term_sym = suffixed(ident, "Term");
            tokens.extend(quote! {
                #[derive(Clone, PartialEq, Eq, Hash, Debug)]
                pub enum #term_sym {
                    #term_tokens
                }
            });

            let mut mut_tokens = proc_macro2::TokenStream::default();
            for ident in terms {
                let term_type = &term_types[ident];
                mut_tokens.extend(quote![#ident(&'a mut #term_type),]);
            }
            let term_mut_sym = suffixed(ident, "TermMut");

            let mut storage_variant_tokens = proc_macro2::TokenStream::default();
            let mut write_to_storage_tokens = proc_macro2::TokenStream::default();
            let mut read_from_storage_tokens = proc_macro2::TokenStream::default();
            let mut ref_storage_tokens = proc_macro2::TokenStream::default();
            let mut possible_storage_variants_tokens = proc_macro2::TokenStream::default();
            let mut mut_storage_tokens = proc_macro2::TokenStream::default();

            for ident in terms {
                let term_type = &term_types[ident];
                storage_variant_tokens.extend(quote! {
                    #term_sym::#ident(_) => #root_variant_sym::#ident,
                });
                write_to_storage_tokens.extend(quote! {
                    #term_sym::#ident(term) => ptr::write(
                        ptr as *mut #node_sym<#term_type>,
                        #node_sym {
                            output: self.output,
                            term,
                        },
                    ),
                });
                read_from_storage_tokens.extend(quote! {
                    #root_variant_sym::#ident => {
                        let read_node = (ptr as *const #node_sym<#term_type>).read();
                        Some(#node_sym {
                            output: read_node.output,
                            term: #term_sym::#ident(read_node.term),
                        })
                    }
                });
                ref_storage_tokens.extend(quote! {
                    #root_variant_sym::#ident => {
                        let ref_node = &*(ptr as *const #node_sym<#term_type>);
                        Some(#node_sym {
                            output: ref_node.output,
                            term: #term_sym::#ident(ref_node.term.clone()),
                        })
                    }
                });
                possible_storage_variants_tokens.extend(quote![#root_variant_sym::#ident,]);
                mut_storage_tokens.extend(quote! {
                    #root_variant_sym::#ident => {
                        let mut_node = &mut *(ptr as *mut #node_sym<#term_type>);
                        Some(#node_mut_sym {
                            output: &mut mut_node.output,
                            term: #term_mut_sym::#ident(&mut mut_node.term)
                        })
                    }
                });
            }

            tokens.extend(quote! {
                pub enum #term_mut_sym<'a> {
                    #mut_tokens
                }

                impl<'a> From<&'a #term_sym> for #term_sym {
                    fn from(value: &'a #term_sym) -> Self {
                        value.clone()
                    }
                }
                unsafe impl PagedStorageItem<#catalog_sym> for #node_sym<#term_sym> {
                    fn storage_variant(&self, _catalog: &mut #catalog_sym) -> #root_variant_sym {
                        match self.term {
                            #storage_variant_tokens
                        }
                    }
                    unsafe fn write_to_storage(self, ptr: *mut u8) {
                        unsafe {
                            match self.term {
                                #write_to_storage_tokens
                            }
                        }
                    }
                    unsafe fn read_from_storage(
                        ptr: *const u8,
                        _catalog: &#catalog_sym,
                        variant: #root_variant_sym,
                    ) -> Option<Self> {
                        unsafe {
                            match variant {
                                #read_from_storage_tokens
                                _ => None
                            }
                        }
                    }
                }
                unsafe impl PagedStorageItemRef<'_, #catalog_sym> for #node_sym<#term_sym> {
                     unsafe fn ref_storage(
                        ptr: *const u8,
                        catalog: &#catalog_sym,
                        variant: <#catalog_sym as PagedStorageCatalog>::Variant,
                    ) -> Option<Self> {
                        unsafe {
                            match variant {
                                #ref_storage_tokens
                                _ => None
                            }
                        }
                    }

                    fn possible_storage_variants(
                        _catalog: &#catalog_sym,
                    ) -> impl Iterator<Item = <#catalog_sym as PagedStorageCatalog>::Variant> + '_ {
                        [#possible_storage_variants_tokens].into_iter()
                    }
                }
                unsafe impl<'a> PagedStorageItemMut<'a, #catalog_sym> for #node_mut_sym<'a, #term_mut_sym<'a>> {
                    unsafe fn mut_storage(
                        ptr: *mut u8,
                        _catalog: &#catalog_sym,
                        variant: <#catalog_sym as PagedStorageCatalog>::Variant,
                    ) -> Option<Self> {
                        unsafe {
                            match variant {
                                #mut_storage_tokens
                                _ => None
                            }
                        }
                    }
                }
            });

            let mut use_vars_into_extend_tokens = proc_macro2::TokenStream::default();
            let mut nonguarding_vars_into_extend_tokens = proc_macro2::TokenStream::default();
            let mut map_tokens = proc_macro2::TokenStream::default();
            let mut rewrite_tokens = proc_macro2::TokenStream::default();

            for ident in terms {
                let mut use_vars_handler = proc_macro2::TokenStream::default();
                let mut nonguarding_vars_handler = proc_macro2::TokenStream::default();
                let mut map_handler = proc_macro2::TokenStream::default();
                let mut rewrite_handler = proc_macro2::TokenStream::default();

                for (i, field) in self.terms[ident].fields.iter().enumerate() {
                    let mut field_handler = proc_macro2::TokenStream::default();

                    let member = match &field.ident {
                        Some(ident) => syn::Member::Named(ident.clone()),
                        None => syn::Member::Unnamed(i.into()),
                    };

                    let field_access = match self.terms[ident].kind {
                        TermKind::Inline => quote![term],
                        TermKind::Unit => unreachable!(),
                        TermKind::Tuple | TermKind::Struct => quote![term.#member],
                    };

                    let mut_field_access = match self.terms[ident].kind {
                        TermKind::Inline => quote![(*term)],
                        TermKind::Unit => unreachable!(),
                        TermKind::Tuple | TermKind::Struct => quote![term.#member],
                    };

                    if !field.is_opaque {
                        field_handler.extend(quote! {
                            #field_access.contained_vars_into_extend(sink);
                        });
                        rewrite_handler.extend(quote! {
                            #mut_field_access = #mut_field_access.map_contained_vars(&mut fun);
                        });
                    }

                    use_vars_handler.extend([field_handler.clone()]);
                    if !field.is_guarding {
                        nonguarding_vars_handler.extend([field_handler]);
                    }

                    let map_action = if field.is_opaque {
                        quote![#field_access.clone()]
                    } else {
                        quote![#field_access.map_contained_vars(&mut fun)]
                    };

                    match self.terms[ident].kind {
                        TermKind::Inline => map_handler.extend([map_action]),
                        TermKind::Unit => unreachable!(),
                        TermKind::Tuple => map_handler.extend(quote![#map_action,]),
                        TermKind::Struct => map_handler.extend(quote![#member: #map_action, ]),
                    }
                }

                use_vars_into_extend_tokens.extend(quote! {
                    #term_sym::#ident(term) => { #use_vars_handler }
                });
                nonguarding_vars_into_extend_tokens.extend(quote! {
                    #term_sym::#ident(term) => { #nonguarding_vars_handler }
                });

                match self.terms[ident].kind {
                    TermKind::Inline => {
                        map_tokens.extend(quote! {
                            #term_sym::#ident(term) => #root_term_sym::#ident(#map_handler),
                        });
                    }
                    TermKind::Unit => {
                        map_tokens.extend(quote! {
                            #term_sym::#ident(term) => #root_term_sym::#ident(#ident),
                        });
                    }
                    TermKind::Tuple => {
                        map_tokens.extend(quote! {
                            #term_sym::#ident(term) => #root_term_sym::#ident(#ident(#map_handler)),
                        });
                    }
                    TermKind::Struct => {
                        map_tokens.extend(quote! {
                            #term_sym::#ident(term) => #root_term_sym::#ident(#ident {
                                #map_handler
                            }),
                        });
                    }
                }
                rewrite_tokens.extend(quote! {
                    #term_mut_sym::#ident(term) => {
                        #rewrite_handler
                    }
                });
            }

            tokens.extend(quote! {
                impl IndexedTermRef<#catalog_sym> for #term_sym {
                    fn use_vars_into_extend(&self, sink: &mut impl Extend<#var_type>) {
                        match self {
                            #use_vars_into_extend_tokens
                        }
                    }

                    fn nonguarding_vars_into_extend(&self, sink: &mut impl Extend<#var_type>) {
                        match self {
                            #nonguarding_vars_into_extend_tokens
                        }
                    }

                    fn map(&self, mut fun: impl FnMut(#lit_type) -> #lit_type) -> <#catalog_sym as IndexedCatalog>::Term {
                        match self {
                            #map_tokens
                        }
                    }
                }
            });

            // FIXME: there's a mismatch between the definition for map returning the root expr type
            // and these requiring the same expr type, hence the `if` here, but it would be better
            // to have this for all expr types.
            if term_sym == root_term_sym {
                tokens.extend(quote! {
                    impl IndexedNode<#catalog_sym> for #node_sym<#term_sym> {
                        fn new(output: #lit_type, term: #term_sym) -> Self {
                            #node_sym { output, term }
                        }
                    }

                    impl IndexedNodeRef<#catalog_sym> for #node_sym<#term_sym> {
                        fn output(&self) -> #lit_type {
                            self.output
                        }
                        fn term(&self) -> #term_sym {
                            self.term.clone()
                        }
                        fn map(&self, mut fun: impl FnMut(#lit_type) -> #lit_type) -> #node_sym<#term_sym> {
                            let output = fun(self.output);
                            let term = self.term.map(fun);
                            Node { output, term }
                        }
                    }


                    impl<'a> IndexedNodeMut<#catalog_sym> for #node_mut_sym<'a, #term_mut_sym<'a>> {
                        fn rewrite(&mut self, mut fun: impl FnMut(#lit_type) -> #lit_type) {
                            // TODO can generate code that avoids cloning of opaque parts
                            *self.output = fun(*self.output);
                            match &mut self.term {
                                #rewrite_tokens
                            }
                        }
                    }
                });
            }
        }
    }
}
