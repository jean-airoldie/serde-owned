#![crate_type = "proc-macro"]

extern crate proc_macro;

use {
    proc_macro::TokenStream,
    proc_macro2::TokenStream as TokenStream2,
    quote::{format_ident, quote},
    syn::{
        Data, Fields, Ident,
        Meta::{List, NameValue},
        NestedMeta::Meta,
        Path,
    },
};

use std::fmt;

#[derive(Copy, Clone)]
struct Symbol(&'static str);

const SERDE_OWNED: Symbol = Symbol("serde_owned");
const CRATE: Symbol = Symbol("crate");

impl PartialEq<Symbol> for Ident {
    fn eq(&self, word: &Symbol) -> bool {
        self == word.0
    }
}

impl<'a> PartialEq<Symbol> for &'a Ident {
    fn eq(&self, word: &Symbol) -> bool {
        *self == word.0
    }
}

impl PartialEq<Symbol> for Path {
    fn eq(&self, word: &Symbol) -> bool {
        self.is_ident(word.0)
    }
}

impl<'a> PartialEq<Symbol> for &'a Path {
    fn eq(&self, word: &Symbol) -> bool {
        self.is_ident(word.0)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.0)
    }
}

fn get_serde_owned_meta_items(attr: &syn::Attribute) -> Result<Vec<syn::NestedMeta>, String> {
    if attr.path != SERDE_OWNED {
        return Ok(Vec::new());
    }

    match attr.parse_meta() {
        Ok(List(meta)) => Ok(meta.nested.into_iter().collect()),
        Ok(_) => Err(format!("expected #[{}(...)]", SERDE_OWNED)),
        Err(err) => Err(format!("{}", err)),
    }
}

fn get_lit_str(attr_name: Symbol, lit: &syn::Lit) -> Result<&syn::LitStr, String> {
    get_lit_str2(attr_name, attr_name, lit)
}

fn get_lit_str2(
    attr_name: Symbol,
    meta_item_name: Symbol,
    lit: &syn::Lit,
) -> Result<&syn::LitStr, String> {
    if let syn::Lit::Str(lit) = lit {
        Ok(lit)
    } else {
        Err(format!(
            "expected serde_proxy {} attribute to be a string: `{} = \"...\"`",
            attr_name, meta_item_name
        ))
    }
}

// Generate serialize & deserialize bound for serde serialization to
// overwrite the default `where T: _serde::Serialize + _serde::Deserialize` bound.
// https://github.com/serde-rs/serde/issues/938
fn serde_bound(ty: &syn::Type, krate: &Ident) -> TokenStream2 {
    let ser_bound = {
        let bound = quote! { <#ty as #krate::SerdeOwned>::Proxy: #krate::serde::Serialize };
        let bound_str = bound.to_string();
        quote! { #[serde(bound(serialize = #bound_str))] }
    };

    let de_bound = {
        let bound =
            quote! { <#ty as #krate::SerdeOwned>::Proxy: #krate::serde::de::DeserializeOwned};
        let bound_str = bound.to_string();
        quote! { #[serde(bound(deserialize = #bound_str))] }
    };

    quote! {
        #ser_bound
        #de_bound
    }
}

struct Helpers {
    // serde_proxy krate identifier.
    krate: Option<Ident>,
}

impl Helpers {
    fn from_ast(input: &syn::DeriveInput) -> Self {
        let mut krate = None;

        for meta_item in input
            .attrs
            .iter()
            .flat_map(|attr| get_serde_owned_meta_items(attr))
            .flatten()
        {
            match &meta_item {
                // Parse `#[serde_proxy(krate = "foo")]`
                Meta(NameValue(m)) if m.path == CRATE => {
                    let s = get_lit_str(CRATE, &m.lit).unwrap();
                    let ident = format_ident!("{}", &s.value());
                    krate.replace(ident);
                }
                // FIXME need better error message.
                _ => panic!("unexpected attribute"),
            }
        }

        Self { krate }
    }
}

fn derive_proxy_named_struct(
    krate: &Ident,
    generics: &syn::Generics,
    ident: &Ident,
    proxy_ident: &Ident,
    fields: syn::FieldsNamed,
) -> TokenStream2 {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let proxy_fields: Vec<TokenStream2> = fields
        .named
        .iter()
        .map(|field| {
            let attrs = &field.attrs;
            let ident = &field.ident;
            let ty = &field.ty;

            let serde_bound = serde_bound(ty, krate);

            quote! { #(#attrs)* #serde_bound #ident: <#ty as #krate::SerdeOwned>::Proxy }
        })
        .collect();

    let serde_crate_str = quote! { #krate::serde }.to_string();

    let proxy_ty = quote! {
        #[derive(#krate::serde::Serialize, #krate::serde::Deserialize)]
        #[serde(crate = #serde_crate_str)]
        #[allow(non_camel_case_types)]
        struct #proxy_ident #ty_generics #where_clause {
            #(#proxy_fields),*
        }
    };

    let from_proxy_fields: Vec<TokenStream2> = fields
        .named
        .iter()
        .map(|field| {
            let ident = &field.ident;
            quote! { #ident: #krate::SerdeOwned::from_proxy(proxy.#ident) }
        })
        .collect();

    let into_proxy_fields: Vec<TokenStream2> = fields
        .named
        .iter()
        .map(|field| {
            let ident = &field.ident;
            quote! { #ident: self.#ident.into_proxy() }
        })
        .collect();

    let proxy_impl = quote! {
        impl #impl_generics #krate::SerdeOwned for #ident #ty_generics #where_clause {
            type Proxy = #proxy_ident #ty_generics;

            unsafe fn from_proxy(proxy: Self::Proxy) -> Self {
                Self {
                    #(#from_proxy_fields),*
                }
            }

            unsafe fn into_proxy(self) -> Self::Proxy {
                Self::Proxy {
                    #(#into_proxy_fields),*
                }
            }
        }
    };

    quote! {
        #proxy_ty
        #proxy_impl
    }
}

fn derive_proxy_unnamed_struct(
    krate: &Ident,
    generics: &syn::Generics,
    ident: &Ident,
    proxy_ident: &Ident,
    fields: syn::FieldsUnnamed,
) -> TokenStream2 {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let proxy_fields: Vec<TokenStream2> = fields
        .unnamed
        .iter()
        .map(|field| {
            let attrs = &field.attrs;
            let ty = &field.ty;

            let serde_bound = serde_bound(ty, krate);

            quote! { #(#attrs)* #serde_bound <#ty as #krate::SerdeOwned>::Proxy }
        })
        .collect();

    let serde_crate_str = quote! { #krate::serde }.to_string();

    let proxy_ty = quote! {
        #[derive(#krate::serde::Serialize, #krate::serde::Deserialize)]
        #[serde(crate = #serde_crate_str)]
        #[allow(non_camel_case_types)]
        struct #proxy_ident #ty_generics (
            #(#proxy_fields),*
        ) #where_clause;
    };

    let nb_fields = fields.unnamed.len();

    let from_proxy_fields: Vec<TokenStream2> = (0..nb_fields)
        .map(|i| {
            let idx = syn::Index::from(i);
            quote! { #krate::SerdeOwned::from_proxy(proxy.#idx) }
        })
        .collect();

    let into_proxy_fields: Vec<TokenStream2> = (0..nb_fields)
        .map(|i| {
            let idx = syn::Index::from(i);
            quote! { self.#idx.into_proxy() }
        })
        .collect();

    let proxy_impl = quote! {
        impl #impl_generics #krate::SerdeOwned for #ident #ty_generics #where_clause {
            type Proxy = #proxy_ident #ty_generics;

            unsafe fn from_proxy(proxy: Self::Proxy) -> Self {
                Self (
                    #(#from_proxy_fields),*
                )
            }

            unsafe fn into_proxy(self) -> Self::Proxy {
                #proxy_ident (
                    #(#into_proxy_fields),*
                )
            }
        }
    };

    quote! {
        #proxy_ty
        #proxy_impl
    }
}

fn derive_proxy_struct(
    krate: &Ident,
    generics: &syn::Generics,
    ident: &Ident,
    proxy_ident: &Ident,
    data: syn::DataStruct,
) -> TokenStream2 {
    match data.fields {
        Fields::Named(fields) => {
            derive_proxy_named_struct(krate, generics, ident, proxy_ident, fields)
        }
        Fields::Unnamed(fields) => {
            derive_proxy_unnamed_struct(krate, generics, ident, proxy_ident, fields)
        }
        Fields::Unit => unimplemented!(),
    }
}

fn derive_proxy_enum(
    krate: &Ident,
    generics: &syn::Generics,
    ident: &Ident,
    proxy_ident: &Ident,
    data: syn::DataEnum,
) -> TokenStream2 {
    let proxy_fields: Vec<TokenStream2> = data
        .variants
        .iter()
        .map(|variant| {
            let attrs = &variant.attrs;
            let ident = &variant.ident;
            let fields = &variant.fields;
            let discriminant = &variant.discriminant;

            match fields {
                Fields::Named(_) => unimplemented!("anonymous struct are not supported"),
                Fields::Unnamed(fields) => {
                    let field = fields.unnamed.first().unwrap();
                    let ty = &field.ty;

                    let serde_bound = serde_bound(ty, krate);

                    quote! { #(#attrs)* #serde_bound #ident(<#ty as #krate::SerdeOwned>::Proxy) }
                }
                Fields::Unit => match discriminant {
                    Some((eq, expr)) => {
                        quote! { #(#attrs)* #ident #eq #expr }
                    }
                    None => {
                        quote! { #(#attrs)* #ident }
                    }
                },
            }
        })
        .collect();

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let serde_crate_str = quote! { #krate::serde }.to_string();

    let proxy_ty = quote! {
        #[derive(#krate::serde::Serialize, #krate::serde::Deserialize)]
        #[serde(crate = #serde_crate_str)]
        #[allow(non_camel_case_types)]
        enum #proxy_ident #ty_generics #where_clause {
            #(#proxy_fields),*
        }
    };

    let from_proxy_fields: Vec<TokenStream2> = data
        .variants
        .iter()
        .map(|variant| {
            let field_ident = &variant.ident;
            let fields = &variant.fields;

            match fields {
                Fields::Named(_) => unimplemented!("anonymous struct are not supported"),
                Fields::Unnamed(_) => {
                    quote! { #proxy_ident::#field_ident(value) =>
                        #ident::#field_ident(#krate::SerdeOwned::from_proxy(value))
                    }
                }
                Fields::Unit => {
                    quote! { #proxy_ident::#field_ident => #ident::#field_ident }
                }
            }
        })
        .collect();

    let into_proxy_fields: Vec<TokenStream2> = data
        .variants
        .iter()
        .map(|variant| {
            let field_ident = &variant.ident;
            let fields = &variant.fields;

            match fields {
                Fields::Named(_) => unimplemented!("anonymous struct are not supported"),
                Fields::Unnamed(_) => {
                    quote! { #ident::#field_ident(value) =>
                        #proxy_ident::#field_ident(value.into_proxy())
                    }
                }
                Fields::Unit => {
                    quote! { #ident::#field_ident => #proxy_ident::#field_ident }
                }
            }
        })
        .collect();

    let proxy_impl = quote! {
        impl #impl_generics #krate::SerdeOwned for #ident #ty_generics #where_clause {
            type Proxy = #proxy_ident #ty_generics;

            unsafe fn from_proxy(proxy: Self::Proxy) -> Self {
                match proxy {
                    #(#from_proxy_fields),*
                }
            }

            unsafe fn into_proxy(self) -> Self::Proxy {
                match self {
                    #(#into_proxy_fields),*
                }
            }
        }
    };

    quote! {
        #proxy_ty
        #proxy_impl
    }
}

#[proc_macro_derive(SerdeOwned, attributes(serde_owned))]
pub fn serde_owned(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    // Parse the helper attributes from the ast.
    let helpers = Helpers::from_ast(&ast);

    let krate = helpers
        .krate
        .unwrap_or_else(|| format_ident!("{}", SERDE_OWNED.0));

    let generics = ast.generics;
    let ident = ast.ident;
    let proxy_ident = format_ident!("_SERDE_PROXY_FOR_{}", ident);

    let impl_ = match ast.data {
        Data::Struct(data) => derive_proxy_struct(&krate, &generics, &ident, &proxy_ident, data),
        Data::Enum(data) => derive_proxy_enum(&krate, &generics, &ident, &proxy_ident, data),
        Data::Union(_data) => unimplemented!(),
    };

    impl_.into()
}
