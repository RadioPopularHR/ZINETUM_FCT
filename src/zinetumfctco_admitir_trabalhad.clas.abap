class ZINETUMFCTCO_ADMITIR_TRABALHAD definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods ADMITIR_TRABALHADOR
    importing
      !INPUT type ZINETUMFCTADMITIR_TRABALHADOR2
    exporting
      !OUTPUT type ZINETUMFCTADMITIR_TRABALHADOR1
    raising
      CX_AI_SYSTEM_FAULT
      ZINETUMFCTCX_GFCTEXCEPTION .
  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZINETUMFCTCO_ADMITIR_TRABALHAD IMPLEMENTATION.


  method ADMITIR_TRABALHADOR.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'ADMITIR_TRABALHADOR'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZINETUMFCTCO_ADMITIR_TRABALHAD'
    logical_port_name   = logical_port_name
  ).

  endmethod.
ENDCLASS.
