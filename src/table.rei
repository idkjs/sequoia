/** SQL tables */

/** The type of tables. */

type t('a);

/** Define a table with the given name. */

let called: string => t('a);

/** Returns the name of a table. */

let name: t('a) => string;
