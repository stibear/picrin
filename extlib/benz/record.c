/**
 * See Copyright Notice in picrin.h
 */

#include "picrin.h"

struct pic_record *
pic_make_record(pic_state *pic, struct pic_record *type)
{
  struct pic_record *rec;
  struct pic_dict *data = pic_make_dict(pic);

  rec = (struct pic_record *)pic_obj_alloc(pic, sizeof(struct pic_record), PIC_TT_RECORD);
  rec->data = data;
  rec->type = type;

  if (rec->type == NULL) {
    rec->type = rec;
  }

  return rec;
}

struct pic_record *
pic_record_type(pic_state PIC_UNUSED(*pic), struct pic_record *rec)
{
  return rec->type;
}

pic_value
pic_record_ref(pic_state *pic, struct pic_record *rec, pic_sym *slot)
{
  if (! pic_dict_has(pic, rec->data, slot)) {
    pic_errorf(pic, "slot named ~s is not found for record: ~s", pic_obj_value(slot), pic_obj_value(rec));
  }
  return pic_dict_ref(pic, rec->data, slot);
}

void
pic_record_set(pic_state *pic, struct pic_record *rec, pic_sym *slot, pic_value val)
{
  pic_dict_set(pic, rec->data, slot, val);
}

static pic_value
pic_record_make_record(pic_state *pic)
{
  struct pic_record * rec;
  pic_value rectype;

  pic_get_args(pic, "o", &rectype);

  pic_assert_type(pic, rectype, record);

  rec = pic_make_record(pic, pic_record_ptr(rectype));

  return pic_obj_value(rec);
}

static pic_value
pic_record_record_p(pic_state *pic)
{
  pic_value rec;

  pic_get_args(pic, "o", &rec);

  return pic_bool_value(pic_record_p(rec));
}

static pic_value
pic_record_record_type(pic_state *pic)
{
  struct pic_record *rec;

  pic_get_args(pic, "r", &rec);

  return pic_obj_value(pic_record_type(pic, rec));
}

static pic_value
pic_record_record_ref(pic_state *pic)
{
  struct pic_record *rec;
  pic_sym *slot;

  pic_get_args(pic, "rm", &rec, &slot);

  return pic_record_ref(pic, rec, slot);
}

static pic_value
pic_record_record_set(pic_state *pic)
{
  struct pic_record *rec;
  pic_sym *slot;
  pic_value val;

  pic_get_args(pic, "rmo", &rec, &slot, &val);

  pic_record_set(pic, rec, slot, val);

  return pic_undef_value();
}

void
pic_init_record(pic_state *pic)
{
  pic_defun(pic, "make-record", pic_record_make_record);
  pic_defun(pic, "record?", pic_record_record_p);
  pic_defun(pic, "record-type", pic_record_record_type);
  pic_defun(pic, "record-ref", pic_record_record_ref);
  pic_defun(pic, "record-set!", pic_record_record_set);
  pic_define(pic, "<record-type>", pic_obj_value(pic_make_record(pic, NULL)));
}
