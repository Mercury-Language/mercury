:- module intermod_record2.

:- interface.

:- type record.

:- func field(record) = int.

:- implementation.

:- type record
	---> record(
		field :: int
	).

