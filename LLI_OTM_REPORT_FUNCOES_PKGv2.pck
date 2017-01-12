create or replace
PACKAGE              "LLI_OTM_REPORT_FUNCOES_PKG" IS

-- +=================================================================+
-- |          Copyright (c) 2012 LOGIN, Rio de Janeiro, Brasil       |
-- |                       All rights reserved.                      |
-- +=================================================================+
-- | FILENAME                                                        |
-- |   <lli_otm_report_funcoes_pkg>                                  |
-- |                                                                 |
-- | PURPOSE                                                         |
-- |    Funcoes auxiliares na geracao de relatorios do OTM, visando  |
-- |    melhoria de performance.                                     |
-- |                                                                 |
-- | DESCRIPTION                                                     |
-- |    Recuperar colunas de relatorios nas tabelas referentes a     |
-- |    involved_party, location, refnum, remark e status do         |
-- |    Shipment e Order Release                                     |
-- |                                                                 |
-- | PARAMETERS                                                      |
-- |                                                                 |
-- |                                                                 |
-- | CREATED BY                                                      |
-- |    Karina Campos                  /     18/01/2012              |
-- |                                                                 |
-- | UPDATED BY                                                      |
-- |    Ricardo Macedo                 /     30/01/2012              |
-- |    Karina Campos                  /     14/02/2012              |
-- |    Karina Campos                  /     19/03/2012              |
-- |    Orleans Magalhaes              /     26/03/2012              |
-- |    Regiane Piza                   /     30/04/2013              |
-- |    Karina Campos                  /     29/04/2014              |
-- |    Regiane Piza                   /     10/07/2014  OS 88225    |
-- |    Regiane Piza                   /     11/08/2014  OS 86009    |
-- |    Karina Campos                  /     19/05/2015  OS 91238    |
-- |    Karina Campos                  /     25/11/2015 OS 143937    |
-- |    Ricardo Alves                  /     11/05/2016 OS 162578    |
-- |    Ricardo Alves                  /     22/06/2016 OS 165676    |
-- |    Fernanda Senna                 /     07/07/2016 OS 167226    |
-- |    Ricardo Alves                  /     11/07/2016 OS 167076    |
-- |    Ricardo Alves                  /     23/11/2016 OS 179633    |
-- |    Marcio Mello                   /     04/01/2017 OS 183204    |
-- +=================================================================+

  FUNCTION recupera_shipment_refnum(p_shipment IN shipment_refnum.shipment_gid%TYPE,
                                    p_refnum_qual_gid IN shipment_refnum.shipment_refnum_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_shipment_remark(p_shipment IN shipment_remark.shipment_gid%TYPE,
                                    p_remark_qual_gid IN shipment_remark.remark_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_shipment_sip(p_shipment IN shipment_involved_party.shipment_gid%TYPE,
                                 p_involved_party IN shipment_involved_party.involved_party_qual_gid%TYPE,
                                 p_cl IN VARCHAR2) RETURN VARCHAR2;

  FUNCTION recupera_shipment_status(p_shipment IN shipment_status.shipment_gid%TYPE,
                                    p_status_type_gid IN shipment_status.status_type_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_navio_coleta(p_shipment in shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_viagem_coleta(p_shipment in shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_navio_entrega(p_shipment in shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_viagem_entrega(p_shipment in shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_status_container(p_shipment IN shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_order_release_sip(p_order_release IN order_release_inv_party.order_release_gid%TYPE,
                                      p_involved_party IN order_release_inv_party.involved_party_qual_gid%TYPE,
                                      p_cl IN VARCHAR2) RETURN VARCHAR2;

  FUNCTION recupera_pedido_refnum(p_order_release IN order_release_refnum.order_release_gid%TYPE,
                                  p_refnum_qual_gid IN order_release_refnum.order_release_refnum_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_pedido_remark(p_order_release IN order_release_remark.order_release_gid%TYPE,
                                  p_remark_qual_gid IN order_release_remark.remark_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_pedido_status(p_order_release in order_release_status.order_release_gid%TYPE,
                                    p_status_type_gid in order_release_status.status_type_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_quote_remark (p_quote_gid            in quote_remark.quote_gid%TYPE,
                                  p_remark_qualifier_gid in quote_remark.remark_qualifier_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_quote_c_option_remark (p_quote_gid            in quote_cost_option_remark.quote_gid%TYPE,
                                           p_cost_option_sequence in quote_cost_option_remark.cost_option_sequence%TYPE,
                                           p_remark_qualifier_gid in quote_cost_option_remark.remark_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_quote_c_option_ship   (p_quote_gid            in quote_cost_option_shipment.quote_gid%TYPE,
                                           p_cost_option_sequence   in quote_cost_option_shipment.cost_option_sequence%TYPE,
                                           p_shipment_sequence      in quote_cost_option_shipment.shipment_sequence%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_quote_refnum (p_quote_gid             in quote_refnum.quote_gid%TYPE,
                                  p_quote_refnum_qual_gid in quote_refnum.quote_refnum_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_status_ship_unit(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_porto_transbordo(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_navio_transbordo(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_data_deposito_cntr(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_data_devolucao_vazio(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_data_programada(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                    p_class_trecho  in leg_classification.leg_classification_xid%type,
                                    p_inicial_ou_final in varchar2) RETURN VARCHAR2;

  FUNCTION recupera_cpf_cnpj_outros_ownr(p_involved_party_contact_gid  in  shipment_involved_party.involved_party_contact_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_nro_docto_ctac_bl(p_shipment_gid in shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_status_class_trecho(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                        p_class_trecho  in leg_classification.leg_classification_xid%type,
                                        p_status_type_gid in shipment_status.status_type_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_data_evento(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                p_evento             in IE_SHIPMENTSTATUS.STATUS_CODE_GID%type,
                                p_min_ou_max         in varchar2) RETURN VARCHAR2;

  FUNCTION recupera_navio_viagem_transb(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_location_xid(p_location_gid in location.location_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_ISO_container(p_EquimentReferenteUnitGid IN equip_group_equip_ref_unit.equipment_reference_unit_gid%type,
                                  p_EquimentGroupGid         IN equip_group_equip_ref_unit.equipment_group_gid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_ck_reefer_container(p_EquimentReferenteUnitGid IN equip_group_equip_ref_unit.equipment_reference_unit_gid%type,
                                        p_EquimentGroupGid         IN equip_group_equip_ref_unit.equipment_group_gid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_ship_group_refnum(p_ship_group_gid  IN ship_group_refnum.ship_group_gid%TYPE,
                                      p_refnum_qual_gid IN ship_group_refnum.ship_group_refnum_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_IMO  (p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_cidade_origem_trecho(p_ship_unit_xid   in ship_unit.ship_unit_xid%TYPE,
                                         p_class_trecho    in leg_classification.leg_classification_xid%type) RETURN VARCHAR2;

  FUNCTION recupera_cidade_destino_trecho(p_ship_unit_xid   in ship_unit.ship_unit_xid%TYPE,
                                         p_class_trecho    in leg_classification.leg_classification_xid%type) RETURN VARCHAR2;

  FUNCTION recupera_destinatario_ctms(p_shipment              IN shipment_remark.shipment_gid%TYPE,
                                      p_cnpj_razao_social  IN varchar2) RETURN VARCHAR2;

  FUNCTION recupera_shipment_transbordo(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                        p_seq_transbordo     IN number) RETURN VARCHAR2;

  FUNCTION recupera_data_program_shipment(p_shipment_gid           in shipment.shipment_gid%TYPE,
                                         p_arrival_or_departure   in varchar,
                                         p_stop_inicial_ou_final  in varchar2) RETURN VARCHAR2;

  FUNCTION recupera_data_evento_shipment(p_shipmentt_gid      in shipment.shipment_gid%TYPE,
                                         p_evento             in IE_SHIPMENTSTATUS.STATUS_CODE_GID%type,
                                         p_min_ou_max         in varchar2) RETURN VARCHAR2;

  FUNCTION recupera_navio_viagem_shipment(p_shipment_gid in shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_dt_program_fim_viagem(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_dt_saida_nav_schedul(p_shipment_gid in shipment.shipment_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_status_ship_unit_BIC(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_shipment_vessel_princ(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_location_refnum(p_location        IN location_refnum.location_gid%TYPE,
                                    p_refnum_qual_gid IN location_refnum.location_refnum_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_rem_maritima_com_POD(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_data_evento_trecho(p_class_trecho       in shipment_refnum.shipment_refnum_value%TYPE,
                                       p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                       p_evento             in IE_SHIPMENTSTATUS.STATUS_CODE_GID%type,
                                       p_min_ou_max         in varchar2) RETURN VARCHAR2;

  FUNCTION rategeo_sell_refnum_shipunit(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                       p_refnum_qual_gid    in rate_geo_refnum.rate_geo_refnum_qual_gid%type) RETURN VARCHAR2;


  FUNCTION recup_motiv_reprog_tender_rodo(p_ship_unit_xid  in ship_unit.ship_unit_xid%TYPE,
                                          p_class_leg      in leg_classification.leg_classification_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recup_quote_cost_option_remark(p_quote_gid        in quote_cost_option_remark.quote_gid%TYPE,
                                          p_cost_option_seq  in quote_cost_option_remark.cost_option_sequence%type,
                                          p_remark_qual_gid  in quote_cost_option_remark.remark_qual_gid%type) RETURN VARCHAR2;

  FUNCTION recup_shipment_coleta_entrega(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                         p_class_trecho  in shipment_refnum.shipment_refnum_qual_gid%type) RETURN VARCHAR2;

  FUNCTION recup_dt_status_ship_unit_BIC(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE)  RETURN VARCHAR2;

  FUNCTION recupera_valor_demurrage (P_quote_Xid                   in quote.quote_Xid%type,
                                     P_quote_cost_option_sequence  in quote_cost_option.cost_option_sequence%type,
                                     P_equipment_group_gid         in quote_cost_option_equipment.equipment_group_gid%type) RETURN VARCHAR2;

  FUNCTION recupera_NotaFiscal_PRODUTO(P_shipment_gid                in shipment.shipment_gid%type) RETURN VARCHAR2;

  FUNCTION recupera_Valor_NFiscal_PRODUTO(P_shipment_gid                in shipment.shipment_gid%type) RETURN VARCHAR2;

  FUNCTION recupera_data_ETB(p_ShipmentGiddMarPrincipal  shipment.shipment_gid%type) return varchar2;

  FUNCTION recupera_equipment_type_remark(p_equipment_type_gid IN glogowner.equipment_type_remark.equipment_type_gid%TYPE,
                                          p_remark_qual_gid    IN glogowner.equipment_type_remark.remark_qual_gid%TYPE) RETURN VARCHAR2;

  FUNCTION codigo_proposta_comercial(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_data_status_ship_unit(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2;

  FUNCTION recupera_dt_viagem_orig_dest(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                        p_porto IN VARCHAR2,
                                        P_ETS_ETA IN VARCHAR2,
                                        p_class_trecho IN VARCHAR2) RETURN VARCHAR2;


END LLI_OTM_REPORT_FUNCOES_PKG;
/
create or replace
PACKAGE BODY              "LLI_OTM_REPORT_FUNCOES_PKG" IS


  FUNCTION recupera_shipment_refnum(p_shipment IN shipment_refnum.shipment_gid%TYPE,
                                    p_refnum_qual_gid IN shipment_refnum.shipment_refnum_qual_gid%TYPE)
           RETURN VARCHAR2 AS

    is_shipment shipment_refnum.shipment_refnum_value%TYPE;

  BEGIN
    BEGIN
      SELECT MAX(shipment_refnum_value)
        INTO is_shipment
        FROM shipment_refnum
       WHERE shipment_refnum_qual_gid = p_refnum_qual_gid
         AND shipment_gid = p_shipment;
    EXCEPTION
      WHEN OTHERS THEN
        is_shipment := ' ';
    END;

    RETURN is_shipment;

  END recupera_shipment_refnum;

  ---

  FUNCTION recupera_shipment_remark(p_shipment IN shipment_remark.shipment_gid%TYPE,
                                    p_remark_qual_gid IN shipment_remark.remark_qual_gid%TYPE)
           RETURN VARCHAR2 AS

    is_shipment shipment_remark.remark_text%TYPE;

  BEGIN
    BEGIN
      SELECT MAX(remark_text)
        INTO is_shipment
        FROM shipment_remark
       WHERE remark_qual_gid = p_remark_qual_gid
         AND shipment_gid = p_shipment;
    EXCEPTION
      WHEN OTHERS THEN
        is_shipment := ' ';
    END;

    RETURN is_shipment;

  END recupera_shipment_remark;

  ---

  FUNCTION recupera_shipment_sip(p_shipment IN shipment_involved_party.shipment_gid%TYPE,
                                 p_involved_party IN shipment_involved_party.involved_party_qual_gid%TYPE,
                                 p_cl IN VARCHAR2)
           RETURN VARCHAR2 AS

    is_shipment VARCHAR2(200);

  BEGIN
    /*
    Quando: p_cl = "C", recuperar: shipment_involved_party.involved_party_contact_gid
            p_cl = "L", recuperar: location.location_name
            -- Ricardo Macedo, em 30/01/2012.
            p_cl = "S", recuperar: nvl(contact.first_name, contact.contact_xid) - SACADO
            p_cl = "E", recuperar: nvl(contact.first_name, contact.contact_xid) - EMBARCADOR
    */

    IF p_cl = 'C' THEN
      BEGIN
        SELECT SUBSTR(a.involved_party_contact_gid, instr(a.involved_party_contact_gid, '.') + 1,
               LENGTH(a.involved_party_contact_gid) - instr(a.involved_party_contact_gid, '.'))
          INTO is_shipment
          FROM shipment_involved_party a,
               contact c,
               location l
         WHERE a.involved_party_qual_gid = p_involved_party
           AND a.involved_party_contact_gid = c.contact_gid
           AND l.location_gid = c.location_gid
           AND a.shipment_gid = p_shipment;
      EXCEPTION
        WHEN OTHERS THEN
          is_shipment := ' ';
      END;
    ELSIF p_cl = 'L' THEN --ELSE -- Ricardo Macedo, em 30/01/2012.
      BEGIN
        SELECT MAX(l.location_name)
          INTO is_shipment
          FROM shipment_involved_party a,
               contact c,
               location l
         WHERE a.involved_party_qual_gid = p_involved_party
           AND a.involved_party_contact_gid = c.contact_gid
           AND l.location_gid = c.location_gid
           AND a.shipment_gid = p_shipment;
      EXCEPTION
        WHEN OTHERS THEN
          is_shipment := ' ';
      END;
    -- Ricardo Macedo, em 30/01/2012.
    ELSIF p_cl IN ('S', 'E') THEN
      BEGIN
        SELECT MAX(nvl(c.first_name, c.contact_xid))
          INTO is_shipment
          FROM shipment s,
               shipment_involved_party a,
               contact c,
               location l
         WHERE a.involved_party_qual_gid = p_involved_party
           AND a.involved_party_contact_gid = c.contact_gid
           AND l.location_gid = c.location_gid
           AND a.shipment_gid = p_shipment
           AND a.shipment_gid = s.shipment_gid;
      EXCEPTION
        WHEN OTHERS THEN
          is_shipment := ' ';
      END;
    END IF;

    RETURN is_shipment;

  END recupera_shipment_sip;

  ---

  FUNCTION recupera_shipment_status(p_shipment in shipment_status.shipment_gid%TYPE,
                                    p_status_type_gid in shipment_status.status_type_gid%TYPE)
           RETURN VARCHAR2 AS

    is_shipment shipment_status.status_value_gid%TYPE;

  BEGIN
    BEGIN
      SELECT SUBSTR(status_value_gid, instr(status_value_gid, '.') + 1,
             LENGTH(status_value_gid) - instr(status_value_gid, '.'))
        INTO is_shipment
        FROM shipment_status
       WHERE status_type_gid = p_status_type_gid
         AND shipment_gid = p_shipment;
    EXCEPTION
      WHEN OTHERS THEN
        is_shipment := ' ';
    END;

    RETURN is_shipment;

  END recupera_shipment_status;

  ---
  -- Karina Santos, em 14/02/2012
  FUNCTION recupera_navio_coleta(p_shipment in shipment.shipment_gid%TYPE)
           RETURN VARCHAR2 AS

    is_info VARCHAR2(120);

  l_ship_unit    shipment_refnum.shipment_refnum_value%TYPE;
  l_class_trecho shipment_refnum.shipment_refnum_value%TYPE;

  BEGIN

    -- Verifica Ship_Unit
    l_ship_unit := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.SHIP_UNIT_PEDIDO');

    -- Verifica Classifica? Trecho
    l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');

    if l_class_trecho in ('COLETA_MARITIMA',
                          'MARITIMA',
                          'MARITIMA_COM_COLETA',
                          'MARITIMA_COM_COLETA_TRANSBORDO',
                          'MARITIMA_TRANSBORDO') then

        BEGIN
          SELECT SUBSTR(VESSEL_GID, instr(VESSEL_GID, '.') + 1, LENGTH(VESSEL_GID) - instr(VESSEL_GID, '.'))
            INTO is_info
            FROM SHIPMENT
           WHERE SHIPMENT_XID = ( SELECT MIN(SHP.SHIPMENT_XID)
                                    FROM SHIPMENT                     SHP,
                                         SHIPMENT_S_EQUIPMENT_JOIN    SHE,
                                         VIEW_SHIPMENT_ORDER_RELEASE  VSO,
                                         SHIP_UNIT                    SUT,
                                         S_SHIP_UNIT                  SSU,
                                         S_EQUIPMENT_S_SHIP_UNIT_JOIN SEJ,
                                         SHIP_UNIT_LINE               SLI,
                                         ORDER_RELEASE_LINE           ORR,
                                         shipment_status              SST
                                   WHERE ORR.ORDER_RELEASE_LINE_GID = SLI.ORDER_RELEASE_LINE_GID
                                     AND SLI.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SSU.S_SHIP_UNIT_GID = SEJ.S_SHIP_UNIT_GID
                                     AND SEJ.S_EQUIPMENT_GID = SHE.S_EQUIPMENT_GID
                                     AND SSU.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SUT.ORDER_RELEASE_GID = VSO.ORDER_RELEASE_GID
                                     AND VSO.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SHE.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     --
                                     AND SHP.PERSPECTIVE = 'B'
                                     AND SUT.SHIP_UNIT_XID =  l_ship_unit
                                     AND SHP.TRANSPORT_MODE_GID = 'VESSEL-CH'
                                     AND SST.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SST.status_type_gid = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                     AND SST.status_value_gid = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                     and EXISTS (select order_release_gid
                                                       from order_release_status
                                                       where status_type_gid = 'LOGPLAN.INT_CONDICAO_PEDIDO'
                                                         and status_value_gid ='LOGPLAN.CONDICAO_PEDIDO_PLANEJADO'
                                                         and order_release_gid = VSO.ORDER_RELEASE_GID) );
        EXCEPTION
          WHEN OTHERS THEN
            is_info := ' ';
        END;

    else
      is_info := ' ';
    end if;

    RETURN is_info;

  END recupera_navio_coleta;

  ---
  -- Karina Santos, em 14/02/2012
  FUNCTION recupera_viagem_coleta(p_shipment in shipment.shipment_gid%TYPE)
           RETURN VARCHAR2 AS

    is_info VARCHAR2(120);

  l_ship_unit    shipment_refnum.shipment_refnum_value%TYPE;
  l_class_trecho shipment_refnum.shipment_refnum_value%TYPE;

  BEGIN

    -- Verifica Ship_Unit
    l_ship_unit := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.SHIP_UNIT_PEDIDO');

    -- Verifica Classifica? Trecho
    l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');

    if l_class_trecho in ('COLETA_MARITIMA',
                          'MARITIMA',
                          'MARITIMA_COM_COLETA',
                          'MARITIMA_COM_COLETA_TRANSBORDO',
                          'MARITIMA_TRANSBORDO') then

        BEGIN
          SELECT SUBSTR(VOYAGE_GID, instr(VOYAGE_GID, '.') + 1, LENGTH(VOYAGE_GID) - instr(VOYAGE_GID, '.'))
            INTO is_info
            FROM SHIPMENT
           WHERE SHIPMENT_XID = ( SELECT MIN(SHP.SHIPMENT_XID)
                                    FROM SHIPMENT                     SHP,
                                         SHIPMENT_S_EQUIPMENT_JOIN    SHE,
                                         VIEW_SHIPMENT_ORDER_RELEASE  VSO,
                                         SHIP_UNIT                    SUT,
                                         S_SHIP_UNIT                  SSU,
                                         S_EQUIPMENT_S_SHIP_UNIT_JOIN SEJ,
                                         SHIP_UNIT_LINE               SLI,
                                         ORDER_RELEASE_LINE           ORR,
                                         shipment_status              SST
                                   WHERE ORR.ORDER_RELEASE_LINE_GID = SLI.ORDER_RELEASE_LINE_GID
                                     AND SLI.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SSU.S_SHIP_UNIT_GID = SEJ.S_SHIP_UNIT_GID
                                     AND SEJ.S_EQUIPMENT_GID = SHE.S_EQUIPMENT_GID
                                     AND SSU.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SUT.ORDER_RELEASE_GID = VSO.ORDER_RELEASE_GID
                                     AND VSO.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SHE.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     --
                                     AND SHP.PERSPECTIVE = 'B'
                                     AND SUT.SHIP_UNIT_XID =  l_ship_unit
                                     AND SHP.TRANSPORT_MODE_GID = 'VESSEL-CH'
                                     AND SST.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SST.status_type_gid = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                     AND SST.status_value_gid = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                     and EXISTS (select order_release_gid
                                                       from order_release_status
                                                       where status_type_gid = 'LOGPLAN.INT_CONDICAO_PEDIDO'
                                                         and status_value_gid ='LOGPLAN.CONDICAO_PEDIDO_PLANEJADO'
                                                         and order_release_gid = VSO.ORDER_RELEASE_GID) );
        EXCEPTION
          WHEN OTHERS THEN
            is_info := ' ';
        END;

    else
      is_info := ' ';
    end if;

    RETURN is_info;

  END recupera_viagem_coleta;

  ---
  -- Karina Santos, em 14/02/2012
  FUNCTION recupera_navio_entrega(p_shipment in shipment.shipment_gid%TYPE)
           RETURN VARCHAR2 AS

    is_info VARCHAR2(120);

  l_ship_unit    shipment_refnum.shipment_refnum_value%TYPE;
  l_class_trecho shipment_refnum.shipment_refnum_value%TYPE;

  BEGIN

    -- Verifica Ship_Unit
    l_ship_unit := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.SHIP_UNIT_PEDIDO');

    -- Verifica Classifica? Trecho
    l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');

    if l_class_trecho in ('ENTREGA_MARITIMA','TRANSBORDO') then

        BEGIN
          SELECT SUBSTR(VESSEL_GID, instr(VESSEL_GID, '.') + 1, LENGTH(VESSEL_GID) - instr(VESSEL_GID, '.'))
            INTO is_info
            FROM SHIPMENT
           WHERE SHIPMENT_XID = ( SELECT MAX(SHP.SHIPMENT_XID)
                                    FROM SHIPMENT                     SHP,
                                         SHIPMENT_S_EQUIPMENT_JOIN    SHE,
                                         VIEW_SHIPMENT_ORDER_RELEASE  VSO,
                                         SHIP_UNIT                    SUT,
                                         S_SHIP_UNIT                  SSU,
                                         S_EQUIPMENT_S_SHIP_UNIT_JOIN SEJ,
                                         SHIP_UNIT_LINE               SLI,
                                         ORDER_RELEASE_LINE           ORR,
                                         shipment_status              SST
                                   WHERE ORR.ORDER_RELEASE_LINE_GID = SLI.ORDER_RELEASE_LINE_GID
                                     AND SLI.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SSU.S_SHIP_UNIT_GID = SEJ.S_SHIP_UNIT_GID
                                     AND SEJ.S_EQUIPMENT_GID = SHE.S_EQUIPMENT_GID
                                     AND SSU.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SUT.ORDER_RELEASE_GID = VSO.ORDER_RELEASE_GID
                                     AND VSO.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SHE.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     --
                                     AND SHP.PERSPECTIVE = 'B'
                                     AND SUT.SHIP_UNIT_XID =  l_ship_unit
                                     AND SHP.TRANSPORT_MODE_GID = 'VESSEL-CH'
                                     AND SST.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SST.status_type_gid = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                     AND SST.status_value_gid = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                     and EXISTS (select order_release_gid
                                                       from order_release_status
                                                       where status_type_gid = 'LOGPLAN.INT_CONDICAO_PEDIDO'
                                                         and status_value_gid ='LOGPLAN.CONDICAO_PEDIDO_PLANEJADO'
                                                         and order_release_gid = VSO.ORDER_RELEASE_GID) );
        EXCEPTION
          WHEN OTHERS THEN
            is_info := ' ';
        END;

    else
      is_info := ' ';
    end if;

    RETURN is_info;

  END recupera_navio_entrega;

  ---
  -- Karina Santos, em 14/02/2012
  FUNCTION recupera_viagem_entrega(p_shipment in shipment.shipment_gid%TYPE)
           RETURN VARCHAR2 AS

    is_info VARCHAR2(120);

  l_ship_unit    shipment_refnum.shipment_refnum_value%TYPE;
  l_class_trecho shipment_refnum.shipment_refnum_value%TYPE;

  BEGIN

    -- Verifica Ship_Unit
    l_ship_unit := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.SHIP_UNIT_PEDIDO');

    -- Verifica Classifica? Trecho
    l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');

    if l_class_trecho in ('ENTREGA_MARITIMA','TRANSBORDO') then

        BEGIN
          SELECT SUBSTR(VOYAGE_GID, instr(VOYAGE_GID, '.') + 1, LENGTH(VOYAGE_GID) - instr(VOYAGE_GID, '.'))
            INTO is_info
            FROM SHIPMENT
           WHERE SHIPMENT_XID = ( SELECT MAX(SHP.SHIPMENT_XID)
                                    FROM SHIPMENT                     SHP,
                                         SHIPMENT_S_EQUIPMENT_JOIN    SHE,
                                         VIEW_SHIPMENT_ORDER_RELEASE  VSO,
                                         SHIP_UNIT                    SUT,
                                         S_SHIP_UNIT                  SSU,
                                         S_EQUIPMENT_S_SHIP_UNIT_JOIN SEJ,
                                         SHIP_UNIT_LINE               SLI,
                                         ORDER_RELEASE_LINE           ORR,
                                         shipment_status              SST
                                   WHERE ORR.ORDER_RELEASE_LINE_GID = SLI.ORDER_RELEASE_LINE_GID
                                     AND SLI.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SSU.S_SHIP_UNIT_GID = SEJ.S_SHIP_UNIT_GID
                                     AND SEJ.S_EQUIPMENT_GID = SHE.S_EQUIPMENT_GID
                                     AND SSU.SHIP_UNIT_GID = SUT.SHIP_UNIT_GID
                                     AND SUT.ORDER_RELEASE_GID = VSO.ORDER_RELEASE_GID
                                     AND VSO.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SHE.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     --
                                     AND SHP.PERSPECTIVE = 'B'
                                     AND SUT.SHIP_UNIT_XID =  l_ship_unit
                                     AND SHP.TRANSPORT_MODE_GID = 'VESSEL-CH'
                                     AND SST.SHIPMENT_GID = SHP.SHIPMENT_GID
                                     AND SST.status_type_gid = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                     AND SST.status_value_gid = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                     and EXISTS (select order_release_gid
                                                       from order_release_status
                                                       where status_type_gid = 'LOGPLAN.INT_CONDICAO_PEDIDO'
                                                         and status_value_gid ='LOGPLAN.CONDICAO_PEDIDO_PLANEJADO'
                                                         and order_release_gid = VSO.ORDER_RELEASE_GID) );
        EXCEPTION
          WHEN OTHERS THEN
            is_info := ' ';
        END;

    else
      is_info := ' ';
    end if;

    RETURN is_info;

  END recupera_viagem_entrega;

  ---
  -- Karina Santos, em 14/02/2012
  /* Fun? espec?ca para atender as regras do Relat?rio de Prontid?de Carga (Gap 090) */
  FUNCTION recupera_status_container(p_shipment IN shipment.shipment_gid%TYPE)
           RETURN VARCHAR2 AS

    is_info         VARCHAR2(100);
    l_status_tender shipment_status.status_value_gid%TYPE;
    l_class_trecho  shipment_refnum.shipment_refnum_value%TYPE;
    l_cod_term_orig shipment_refnum.shipment_refnum_value%TYPE;
    l_cod_term_dest shipment_refnum.shipment_refnum_value%TYPE;


  BEGIN

    -- Classifica? Trecho
    l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');
    -- Status do Tender
    l_status_tender := LLI_OTM_REPORT_FUNCOES_PKG.recupera_shipment_status(p_shipment,'LOGPLAN.INT_TENDER');
    -- Terminal de Origem
    l_cod_term_orig := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment,'LOGPUBLICO.TERMINAL_ORIGEM_EDI');
    -- Terminal de Destino
    l_cod_term_dest := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(p_shipment,'LOGPUBLICO.TERMINAL_DESTINO_EDI');

    if l_class_trecho = 'COLETA_MARITIMA' then
      if (l_status_tender = 'TENDER_ACEITO' or l_status_tender = 'TENDER_ACEITO_COM_RESSALVA_APROVADO')
         and l_cod_term_orig is null
         and l_cod_term_dest is null then
        is_info := 'Agendado no terminal';
      elsif l_cod_term_orig is not null and l_cod_term_dest is null then
        is_info := 'Retirado do terminal';
      elsif l_cod_term_dest is not null then
        is_info := 'Depositado no terminal';
      end if;
    else
      is_info := ' ';
    end if;

    if is_info is null then
      is_info := ' ';
    end if;

    RETURN is_info;

  END recupera_status_container;

  FUNCTION recupera_order_release_sip(p_order_release IN order_release_inv_party.order_release_gid%TYPE,
                                      p_involved_party IN order_release_inv_party.involved_party_qual_gid%TYPE,
                                      p_cl IN VARCHAR2)
           RETURN VARCHAR2 AS

    is_order_release VARCHAR2(200);

  BEGIN
    /*
    Quando: p_cl = "C", recuperar: order_release_inv_party.involved_party_contact_gid
            p_cl = "L", recuperar: location.location_name
    */

    IF p_cl = 'C' THEN
      BEGIN
        SELECT SUBSTR(a.involved_party_contact_gid, instr(a.involved_party_contact_gid, '.') + 1,
               LENGTH(a.involved_party_contact_gid) - instr(a.involved_party_contact_gid, '.'))
          INTO is_order_release
          FROM order_release_inv_party a,
               contact c,
               location l
         WHERE a.involved_party_qual_gid = p_involved_party
           AND a.involved_party_contact_gid = c.contact_gid
           AND l.location_gid = c.location_gid
           AND a.order_release_gid = p_order_release;
      EXCEPTION
        WHEN OTHERS THEN
          is_order_release := ' ';
      END;
    ELSIF p_cl = 'L' THEN
      BEGIN
        SELECT MAX(l.location_name)
          INTO is_order_release
          FROM order_release_inv_party a,
               contact c,
               location l
         WHERE a.involved_party_qual_gid = p_involved_party
           AND a.involved_party_contact_gid = c.contact_gid
           AND l.location_gid = c.location_gid
           AND a.order_release_gid = p_order_release;
      EXCEPTION
        WHEN OTHERS THEN
          is_order_release := ' ';
      END;
    END IF;

    RETURN is_order_release;

  END recupera_order_release_sip;
  --
  -- Karina Santos, em 19/03/2012
  FUNCTION recupera_pedido_refnum(p_order_release IN order_release_refnum.order_release_gid%TYPE,
                                    p_refnum_qual_gid IN order_release_refnum.order_release_refnum_qual_gid%TYPE)
           RETURN VARCHAR2 AS

    is_pedido order_release_refnum.order_release_refnum_value%TYPE;

  BEGIN
    BEGIN
      SELECT MAX(order_release_refnum_value)
        INTO is_pedido
        FROM order_release_refnum
       WHERE order_release_refnum_qual_gid = p_refnum_qual_gid
         AND order_release_gid = p_order_release;
    EXCEPTION
      WHEN OTHERS THEN
        is_pedido := ' ';
    END;

    RETURN is_pedido;

  END recupera_pedido_refnum;

  ---
  -- Karina Santos, em 19/03/2012
  FUNCTION recupera_pedido_remark(p_order_release IN order_release_remark.order_release_gid%TYPE,
                                    p_remark_qual_gid IN order_release_remark.remark_qual_gid%TYPE)
           RETURN VARCHAR2 AS

    is_pedido order_release_remark.remark_text%TYPE;

  BEGIN
    BEGIN
      SELECT MAX(SUBSTR(remark_text, instr(remark_text, '.') + 1,
             LENGTH(remark_text) - instr(remark_text, '.')))
        INTO is_pedido
        FROM order_release_remark
       WHERE remark_qual_gid = p_remark_qual_gid
         AND order_release_gid = p_order_release;
    EXCEPTION
      WHEN OTHERS THEN
        is_pedido := ' ';
    END;

    RETURN is_pedido;

  END recupera_pedido_remark;

  ---
  -- Karina Santos, em 19/03/2012
  FUNCTION recupera_pedido_status(p_order_release in order_release_status.order_release_gid%TYPE,
                                    p_status_type_gid in order_release_status.status_type_gid%TYPE)
           RETURN VARCHAR2 AS

    is_pedido order_release_status.status_value_gid%TYPE;

  BEGIN
    BEGIN
      SELECT SUBSTR(status_value_gid, instr(status_value_gid, '.') + 1,
             LENGTH(status_value_gid) - instr(status_value_gid, '.'))
        INTO is_pedido
        FROM order_release_status
       WHERE status_type_gid = p_status_type_gid
         AND order_release_gid = p_order_release;
    EXCEPTION
      WHEN OTHERS THEN
        is_pedido := ' ';
    END;

    RETURN is_pedido;

  END recupera_pedido_status;

  --
  -- Orleans em 22/03/2012
  FUNCTION recupera_quote_remark (p_quote_gid            in quote_remark.quote_gid%TYPE,
                                  p_remark_qualifier_gid in quote_remark.remark_qualifier_gid%TYPE)
           RETURN VARCHAR2 as

  L_remark_text     quote_remark.remark_text%type;

  Begin
     Begin
        SELECT qr.remark_text texto into L_remark_text
          FROM quote_remark qr
         WHERE qr.remark_qualifier_gid        = p_remark_qualifier_gid
           and qr.quote_gid                   = p_quote_gid;
     Exception when others then L_remark_text := null;
     End;

     RETURN L_remark_text;
  End recupera_quote_remark;

  FUNCTION recupera_quote_c_option_remark (p_quote_gid            in quote_cost_option_remark.quote_gid%TYPE,
                                           p_cost_option_sequence in quote_cost_option_remark.cost_option_sequence%TYPE,
                                           p_remark_qualifier_gid in quote_cost_option_remark.remark_qual_gid%TYPE)
           RETURN VARCHAR2 as

  L_remark_text     quote_remark.remark_text%type;

  Begin
     Begin
        SELECT optremark.remark_text INTO L_remark_text
          FROM quote_cost_option_remark optremark
         WHERE optremark.remark_qual_gid       = p_remark_qualifier_gid
           AND optremark.cost_option_sequence  = p_cost_option_sequence
           AND optremark.quote_gid             = p_quote_gid
           AND ROWNUM                          = 1;
     Exception when others then L_remark_text := null;
     End;

     RETURN L_remark_text;
  End recupera_quote_c_option_remark;

  FUNCTION recupera_quote_c_option_ship (p_quote_gid            in quote_cost_option_shipment.quote_gid%TYPE,
                                         p_cost_option_sequence   in quote_cost_option_shipment.cost_option_sequence%TYPE,
                                         p_shipment_sequence      in quote_cost_option_shipment.shipment_sequence%TYPE)
           RETURN VARCHAR2 as

  L_remark_text     quote_remark.remark_text%type;

  Begin
     Begin
        SELECT decode(optship.transport_mode_gid,'TL','RODOVIARIO',
                                                 'VESSEL-CH','MARITIMO',
                                                 'RAIL','FERROVIARIO',
                                                 'FEEDER','MARITIMO') TRANSPORT_MODE
               INTO L_remark_text
          FROM quote_cost_option_shipment optship
         WHERE OPTSHIP.quote_gid             = p_quote_gid
           AND OPTSHIP.cost_option_sequence  = p_cost_option_sequence
           AND OPTSHIP.shipment_sequence     = p_shipment_sequence;
     Exception when others then L_remark_text := null;
     End;

     RETURN L_remark_text;
  End recupera_quote_c_option_ship;

  FUNCTION recupera_quote_refnum (p_quote_gid             in quote_refnum.quote_gid%TYPE,
                                  p_quote_refnum_qual_gid in quote_refnum.quote_refnum_qual_gid%TYPE)

           RETURN VARCHAR2 as

  L_remark_text     quote_remark.remark_text%type;

  Begin
     Begin
        SELECT QUOTE_REFNUM_VALUE into L_remark_text
          FROM quote_refnum
         WHERE quote_refnum_qual_gid = p_quote_refnum_qual_gid
           AND QUOTE_GID             = p_quote_gid;
     Exception when others then L_remark_text := null;
     End;

     RETURN L_remark_text;
  End recupera_quote_refnum;
  --xxx
  FUNCTION recupera_status_ship_unit(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_status          varchar2(100);
    l_status_code_gid bs_status_code.bs_status_code_gid%type;
  --
  BEGIN
    BEGIN
      SELECT STATUS_CODE_GID
        INTO l_status_code_gid
        FROM ( SELECT to_char(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), 'DD/MM/YYYY HH24:MI:SS'),
                      IES.STATUS_CODE_GID--,
                 FROM SS_STATUS_HISTORY SSH,
                      IE_SHIPMENTSTATUS IES
                WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
                  AND SSH.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                             FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                  SHIPMENT_STATUS COND_SHIP
                                             WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                               AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                               AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                               AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                               AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                               )
          ORDER BY 1 DESC)
        WHERE ROWNUM <= 1;
    EXCEPTION
      WHEN OTHERS THEN
        l_status := ' ';
    END;
    --
    IF l_status_code_gid IS NOT NULL THEN
      --
      IF    l_status_code_gid IN ('LOGPUBLICO.RMI', 'LOGPUBLICO.RMT') THEN
        --
        l_status := 'CTNR VAZIO DEVOLVIDO';
        --
      ELSIF l_status_code_gid IN ('LOGPUBLICO.DVC', 'LOGPUBLICO.DMD') THEN
        --
        l_status := 'CTNR RETIRADO PARA ENTREGA';
        --
      ELSIF l_status_code_gid IN ('LOGPUBLICO.DCF', 'LOGPUBLICO.DCFT', 'LOGPUBLICO.DCM', 'LOGPUBLICO.DCMT') THEN
        --
        l_status := 'EM TERMINAL DESTINO';
        --
      ELSIF l_status_code_gid IN ('LOGPUBLICO.LDF', 'LOGPUBLICO.LDFT', 'LOGPUBLICO.LDM', 'LOGPUBLICO.LDMT') THEN
        --
        l_status := 'EM TRANSITO MAR?IMO';
        --
      ELSIF l_status_code_gid IN ('LOGPUBLICO.RFE', 'LOGPUBLICO.RMT') THEN
        --
        l_status := 'AGUARDANDO EMBARQUE NO NAVIO';
        --
      ELSIF l_status_code_gid IN ('LOGPUBLICO.DMD', 'LOGPUBLICO.DVS') THEN
        --
        l_status :=  'CTNR RETIRADO PARA COLETA';
        --
      ELSE
        --
        l_status := NULL;
        --
      END IF;
    END IF;
      --
    RETURN l_status;
    --
  END recupera_status_ship_unit;
  --xxx
  FUNCTION recupera_porto_transbordo(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_porto_transbordo          varchar2(100);
  --
  BEGIN
    BEGIN
      FOR c IN  (SELECT PORT_TRNB.LOCATION_XID PORTO_TRANSB
                   FROM SHIPMENT_REFNUM SHIP_UNIT,
                        SHIPMENT        SHIP_TRNB,
                        SHIPMENT_STATUS COND_SHIP,
                        LOCATION        PORT_TRNB
                  WHERE SHIP_TRNB.SOURCE_LOCATION_GID      =  PORT_TRNB.LOCATION_GID
                    AND COND_SHIP.STATUS_VALUE_GID         IN (SHIP_TRNB.DOMAIN_NAME || '.CONDICAO_SHIPMENT_VALIDO',  SHIP_TRNB.DOMAIN_NAME || '.CONDICAO_SHIPMENT_ERRO_RECAL_DATAS')
                    AND COND_SHIP.STATUS_TYPE_GID          =  SHIP_TRNB.DOMAIN_NAME || '.INT_CONDICAO_SHIPMENT'
                    AND SHIP_TRNB.SHIPMENT_GID             =  COND_SHIP.SHIPMENT_GID
                    AND SHIP_TRNB.IS_PRIMARY               =  'N'
                    AND SHIP_TRNB.TRANSPORT_MODE_GID       =  'VESSEL-CH'
                    AND SHIP_UNIT.SHIPMENT_GID             =  SHIP_TRNB.SHIPMENT_GID
                    AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID =  'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                    AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    =  p_ship_unit_xid ) LOOP
          --
          IF l_porto_transbordo IS NULL THEN
            l_porto_transbordo := C.PORTO_TRANSB;
          ELSE
            l_porto_transbordo := l_porto_transbordo||'/'||C.PORTO_TRANSB;
          END IF;
          --
        END LOOP;
        --
      EXCEPTION
        WHEN OTHERS THEN
          l_porto_transbordo := ' ';
      END;
      --
      RETURN l_porto_transbordo;
      --
  END recupera_porto_transbordo;
  --
  FUNCTION recupera_navio_transbordo(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_navio_transbordo          varchar2(100);
  --
  BEGIN
    BEGIN
      FOR c IN  (SELECT NAVI_TRNB.VESSEL_XID NAVIO_TRANSB
                   FROM SHIPMENT_REFNUM SHIP_UNIT,
                        SHIPMENT        SHIP_TRNB,
                        SHIPMENT_STATUS COND_SHIP,
                        VESSEL          NAVI_TRNB
                  WHERE SHIP_TRNB.VESSEL_GID               =  NAVI_TRNB.VESSEL_GID
                    AND COND_SHIP.STATUS_VALUE_GID         IN (SHIP_TRNB.DOMAIN_NAME || '.CONDICAO_SHIPMENT_VALIDO',  SHIP_TRNB.DOMAIN_NAME || '.CONDICAO_SHIPMENT_ERRO_RECAL_DATAS')
                    AND COND_SHIP.STATUS_TYPE_GID          =  SHIP_TRNB.DOMAIN_NAME || '.INT_CONDICAO_SHIPMENT'
                    AND SHIP_TRNB.SHIPMENT_GID             =  COND_SHIP.SHIPMENT_GID
                    AND SHIP_TRNB.IS_PRIMARY               =  'N'
                    AND SHIP_TRNB.TRANSPORT_MODE_GID       =  'VESSEL-CH'
                    AND SHIP_UNIT.SHIPMENT_GID             =  SHIP_TRNB.SHIPMENT_GID
                    AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID =  'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                    AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    =  p_ship_unit_xid ) LOOP
          --
          IF l_navio_transbordo IS NULL THEN
            l_navio_transbordo := C.NAVIO_TRANSB;
          ELSE
            l_navio_transbordo := l_navio_transbordo||'/'||C.NAVIO_TRANSB;
          END IF;
          --
        END LOOP;
        --
      EXCEPTION
        WHEN OTHERS THEN
          l_navio_transbordo := ' ';
      END;
      --
      RETURN l_navio_transbordo;
      --
  END recupera_navio_transbordo;
  --
  FUNCTION recupera_data_deposito_cntr(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS
  --
    l_data_deposito_conteiner          varchar2(100);
  --
  BEGIN
    BEGIN
      SELECT to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')
        INTO l_data_deposito_conteiner
        FROM SS_STATUS_HISTORY SSH,
             IE_SHIPMENTSTATUS IES
       WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
         AND IES.STATUS_CODE_GID IN ('LOGPUBLICO.RFE','LOGPUBLICO.RMT')
         AND SSH.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                    FROM SHIPMENT_REFNUM SHIP_UNIT,
                                         SHIPMENT_STATUS COND_SHIP
                                   WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                     AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                     AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                     AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                     AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid);
      EXCEPTION
        WHEN OTHERS THEN
          l_data_deposito_conteiner := ' ';
      END;
      --
      RETURN l_data_deposito_conteiner;
      --
  END recupera_data_deposito_cntr;
  --
  FUNCTION recupera_data_devolucao_vazio(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS
  --
    l_data_devolucao_vazio          varchar2(100);
  --
  BEGIN
    BEGIN
      SELECT to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')
        INTO l_data_devolucao_vazio
        FROM SS_STATUS_HISTORY SSH,
             IE_SHIPMENTSTATUS IES
       WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
         AND IES.STATUS_CODE_GID IN ('LOGPUBLICO.RMI','LOGPUBLICO.RMT')
         AND SSH.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                    FROM SHIPMENT_REFNUM SHIP_UNIT,
                                         SHIPMENT_STATUS COND_SHIP
                                   WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                     AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                     AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                     AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                     AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid);
      EXCEPTION
        WHEN OTHERS THEN
          l_data_devolucao_vazio := ' ';
      END;
      --
      RETURN l_data_devolucao_vazio;
      --
  END recupera_data_devolucao_vazio;
  --
  /*FUNCTION recupera_data_programada(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                    p_class_trecho  in leg_classification.leg_classification_xid%type,
                                    p_inicial_ou_final in varchar2) RETURN VARCHAR2 AS
  --
    l_data_programada_ini          varchar2(100);
    l_data_programada_fin          varchar2(100);
    l_data_programada              varchar2(100);
  --
  BEGIN
    BEGIN
      SELECT (to_char(MIN(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_ARRIVAL, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')),
             (to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_ARRIVAL, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS'))
        INTO L_DATA_PROGRAMADA_INI,
             L_DATA_PROGRAMADA_FIN
        FROM SHIPMENT_STOP    SHIP_STOP,
             SHIPMENT_REFNUM  CLAS_TREC,
             LOCATION         LOCA_STOP
       WHERE SHIP_STOP.LOCATION_GID             = LOCA_STOP.LOCATION_GID
         AND CLAS_TREC.SHIPMENT_REFNUM_VALUE    = p_class_trecho
         AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
         AND SHIP_STOP.SHIPMENT_GID             = CLAS_TREC.SHIPMENT_GID
         AND SHIP_STOP.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                          FROM SHIPMENT_REFNUM SHIP_UNIT,
                                               SHIPMENT_STATUS COND_SHIP
                                         WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                           AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                           AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                           AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                           AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid);
      EXCEPTION
        WHEN OTHERS THEN
          l_data_programada_ini := ' ';
      END;
      --
      IF p_inicial_ou_final = 'I' then
        l_data_programada := L_DATA_PROGRAMADA_INI;
      ELSE
        l_data_programada := L_DATA_PROGRAMADA_FIN;
      END IF;
      --
      RETURN l_data_programada;
      --
  END recupera_data_programada;*/

   FUNCTION recupera_data_programada(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                    p_class_trecho  in leg_classification.leg_classification_xid%type,
                                    p_inicial_ou_final in varchar2) RETURN VARCHAR2 AS
  --
    l_data_programada_ini          varchar2(100) := null;
    l_data_programada_fin          varchar2(100) := null;
    l_data_programada              varchar2(100) := null;
    l_status_tender_entrega        varchar2(100) := null;
    l_status_agendamento           varchar2(100) := null;


  BEGIN
    --
    BEGIN
      IF p_class_trecho = 'ENTREGA_MARITIMA' THEN
        --  Ricardo 30/03/2016 - OS  141422
       -- l_status_tender_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', 'LOGPLAN.INT_TENDER');
         l_status_agendamento := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', 'LOGPLAN.AGENDAMENTO_RODOVIARIO');
      else
         l_status_agendamento := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'COLETA_MARITIMA', 'LOGPLAN.AGENDAMENTO_RODOVIARIO');
        --
      END IF;

      IF (l_status_agendamento = ' ') THEN
          l_status_agendamento := 'NAO_TEM_STATUS';
      END IF;


      --
     if (l_status_agendamento = 'NAO_TEM_STATUS') and (p_class_trecho = 'ENTREGA_MARITIMA') then
           l_status_tender_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', 'LOGPLAN.INT_TENDER');
     end if;

     -- RETURN l_status_agendamento;
      /*  --  Ricardo 30/03/2016 - OS  141422
      IF (p_class_trecho <> 'ENTREGA_MARITIMA') or
         (p_class_trecho = 'ENTREGA_MARITIMA' and
          l_status_tender_entrega in ('TENDER_ACEITO','TENDER_ACEITO_COM_RESSALVA_APROVADO')) THEN
      */

      if ( (l_status_agendamento in ('DISPONIVEL_AGR','DISPONIVEL_MANUAL_AGR')) or
             ( ( l_status_agendamento = 'NAO_TEM_STATUS') and
                  ( (p_class_trecho <> 'ENTREGA_MARITIMA') or
                      (p_class_trecho = 'ENTREGA_MARITIMA' and
                      l_status_tender_entrega in ('TENDER_ACEITO','TENDER_ACEITO_COM_RESSALVA_APROVADO') ) )

              )
          ) then
        --
        FOR x IN (SELECT SHIP_UNIT.SHIPMENT_GID
                    FROM SHIPMENT_REFNUM SHIP_UNIT,
                         SHIPMENT_STATUS COND_SHIP
                   WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                     AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                     AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                     AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                     AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid) Loop
          --
          For c IN (SELECT (to_char(MIN(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_ARRIVAL, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')) L_DATA_PROGRAMADA_INI,
                           (to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_ARRIVAL, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')) L_DATA_PROGRAMADA_FIN
                      FROM SHIPMENT_STOP    SHIP_STOP,
                           SHIPMENT_REFNUM  CLAS_TREC,
                           LOCATION         LOCA_STOP
                     WHERE SHIP_STOP.LOCATION_GID             = LOCA_STOP.LOCATION_GID
                       AND CLAS_TREC.SHIPMENT_REFNUM_VALUE    = p_class_trecho
                       AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                       AND SHIP_STOP.SHIPMENT_GID             = CLAS_TREC.SHIPMENT_GID
                       AND SHIP_STOP.SHIPMENT_GID             = x. shipment_gid) LOOP


                   IF p_inicial_ou_final = 'I' then
                      if c.L_data_programada_ini <= nvl(l_data_programada, c.L_DATA_PROGRAMADA_INI) then
                         l_data_programada := c.L_DATA_PROGRAMADA_INI;
                      end if;
                   ELSE
                      if c.L_data_programada_fin >= nvl(l_data_programada, c.L_DATA_PROGRAMADA_fin) then
                         l_data_programada := c.L_DATA_PROGRAMADA_FIN;
                      end if;
                   END IF;
          End Loop;
        End Loop;
        --
      END IF;
      Exception when others then l_data_programada := null;
    END;  --

    RETURN l_data_programada;
      --
  END recupera_data_programada;
  --
  FUNCTION recupera_cpf_cnpj_outros_ownr(p_involved_party_contact_gid  in  shipment_involved_party.involved_party_contact_gid%TYPE) RETURN VARCHAR2 AS
  --
    l_cpf_cnpj_outros          varchar2(100);
  --
  BEGIN
    BEGIN
      SELECT  Decode(INSC_OWNR.LOCATION_REFNUM_VALUE,'CNPJ',
                (Decode(substr(CNPJ_OWNR.LOCATION_REFNUM_VALUE,2),NULL,NULL,
                      REPLACE(REPLACE(REPLACE(To_Char(LPad(REPLACE(substr(CNPJ_OWNR.LOCATION_REFNUM_VALUE,2),''),14 ,'0')
                             ,'00,000,000,0000,00') ,',' ,'.')  ,' ')  ,'.'||Trim(To_Char(Trunc(Mod(LPad(substr(CNPJ_OWNR.LOCATION_REFNUM_VALUE,2)
                             ,14 ,'0') ,1000000)/100) ,'0000'))||'.'
                             ,'/'||Trim(To_Char(Trunc(Mod(LPad(substr(CNPJ_OWNR.LOCATION_REFNUM_VALUE,2) ,14 ,'0') ,1000000)/100) ,'0000'))||'-'))),
                CNPJ_OWNR.LOCATION_REFNUM_VALUE) CPF_CNPJ_OUTROS_ORDEROWNER
        INTO  l_cpf_cnpj_outros
        FROM  CONTACT                      CTCT_OWNR,
              LOCATION                     LOCT_OWNR,
              LOCATION_REFNUM              INSC_OWNR,
              LOCATION_REFNUM              CNPJ_OWNR
        WHERE CTCT_OWNR.CONTACT_GID              = p_involved_party_contact_gid
          AND LOCT_OWNR.LOCATION_GID             = CTCT_OWNR.LOCATION_GID
          AND INSC_OWNR.LOCATION_GID             = LOCT_OWNR.LOCATION_GID
          AND INSC_OWNR.LOCATION_REFNUM_QUAL_GID = 'LOGPUBLICO.TIPO_INSCRICAO'
          AND CNPJ_OWNR.LOCATION_GID             = LOCT_OWNR.LOCATION_GID
          AND CNPJ_OWNR.LOCATION_REFNUM_QUAL_GID =  decode(INSC_OWNR.LOCATION_REFNUM_VALUE,'OUTROS','LOGPUBLICO.NUMERO_CLIENTE','LOGPUBLICO.CPF_CNPJ_OUTROS') -- AJUSTADO EM FUNCAO DO OUTROS
              ;
    EXCEPTION
      WHEN OTHERS THEN
        l_cpf_cnpj_outros := ' ';
    END;
    --
    RETURN l_cpf_cnpj_outros;
    --
  END recupera_cpf_cnpj_outros_ownr;
  --
  FUNCTION recupera_nro_docto_ctac_bl(p_shipment_gid in shipment.shipment_gid%TYPE) RETURN VARCHAR2 AS
  --
    -- Regiane Piza - Alterado em 05/11/2013 para retornar mais de uma nota fiscal concatenada
    l_docto_nro_ctac_bl               varchar2(100);
  --
  BEGIN
    --
    BEGIN
      FOR c in (select shipment_refnum_value docto_nro_serie_comd_ctac_bl
                  from shipment_refnum
                 where shipment_refnum_qual_gid = 'LOGPUBLICO.DOCUMENTO_NRO_SERIE_COMMODITY'
                   and shipment_gid = p_shipment_gid) loop

      IF l_docto_nro_ctac_bl IS NOT NULL THEN
        --
        l_docto_nro_ctac_bl := l_docto_nro_ctac_bl||'/'||SUBSTR(c.docto_nro_serie_comd_ctac_bl,
                                                            5,
                                                           (INSTR(c.docto_nro_serie_comd_ctac_bl,'#SERIE')-5));
      ELSE
        --
        l_docto_nro_ctac_bl := SUBSTR(c.docto_nro_serie_comd_ctac_bl,
                                      5,
                                      (INSTR(c.docto_nro_serie_comd_ctac_bl,'#SERIE')-5));
        --
      END IF;
      --
      END LOOP;
    --
    EXCEPTION
        WHEN OTHERS THEN
          l_docto_nro_ctac_bl := ' ';
    END;
    --
    RETURN nvl(l_docto_nro_ctac_bl,' ');
    --
  END recupera_nro_docto_ctac_bl;
  --
  FUNCTION recupera_status_class_trecho(p_ship_unit_xid   in ship_unit.ship_unit_xid%TYPE,
                                        p_class_trecho    in leg_classification.leg_classification_xid%type,
                                        p_status_type_gid in shipment_status.status_type_gid%TYPE) RETURN VARCHAR2 AS

    l_status          varchar2(100);
    l_shipment_type   varchar2(100);

  --
  BEGIN
    Begin
      FOR c IN  (SELECT SHIP_UNIT.SHIPMENT_GID
                                                     FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                          SHIPMENT_STATUS COND_SHIP
                                                    WHERE SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                      AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                                      AND COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                      AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                      AND COND_SHIP.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID) LOOP
          FOR W IN (SELECT CLAS_TREC.shipment_gid shipment
                      FROM SHIPMENT_REFNUM  CLAS_TREC
                     WHERE CLAS_TREC.SHIPMENT_REFNUM_VALUE    = p_class_trecho
                       AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                       AND CLAS_TREC.SHIPMENT_GID = C.SHIPMENT_GID ) LOOP

                  /* 11/05/2016 OS 162578 - Ricardo */
                  begin
                       select shipment_type_gid
                       into l_shipment_type
                       from shipment where shipment_gid = W.Shipment;
                  exception
                    when OTHERS then
                        return null;
                    end;

                 IF  l_shipment_type = 'TRANSPORT' then

                   IF l_status IS NULL THEN
                      l_status := lli_otm_report_funcoes_pkg.recupera_shipment_status(W.Shipment,p_status_type_gid);
                   ELSE
                      l_status := l_status||'/'||lli_otm_report_funcoes_pkg.recupera_shipment_status(W.shipment,p_status_type_gid);
                   END IF;

                 END IF;

          END LOOP;
          --
      END LOOP;
      EXCEPTION
        WHEN OTHERS THEN
          l_status := ' ';
      END;
      --
      RETURN l_status;
      --
  END recupera_status_class_trecho;
  --
  FUNCTION recupera_data_evento(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                p_evento             in IE_SHIPMENTSTATUS.STATUS_CODE_GID%type,
                                p_min_ou_max         in varchar2) RETURN VARCHAR2 AS
  --
    l_data_evento          varchar2(100);
    l_shipment             SHIPMENT.SHIPMENT_GID%TYPE;
    l_parada_multipla      varchar2(10);

    -- Regiane Piza  - 06/11/2013: correcao para retornar eventos que nao possuem stop associado.
    -- Karina Campos - 11/04/2014: alteracao para considerar a maior data do evento como a do ultimo insert_date.
    --
  BEGIN

    BEGIN
    SELECT DISTINCT SSH.SHIPMENT_GID
          INTO l_shipment
          FROM SS_STATUS_HISTORY SSH,
               IE_SHIPMENTSTATUS IES
         WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
           AND IES.STATUS_CODE_GID  = p_evento
           AND SSH.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                      FROM SHIPMENT_REFNUM SHIP_UNIT,
                                           SHIPMENT_STATUS COND_SHIP
                                     WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                       AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                       AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                       AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                       AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    =  p_ship_unit_xid
                                       );
    EXCEPTION
      WHEN OTHERS THEN
        l_shipment := ' ';
    END;

    -- Verifica se parada Multipla
    BEGIN
      SELECT DECODE(S.SHIPMENT_GID, S.SHIPMENT_GID, 'S', 'N')
        INTO l_parada_multipla
        FROM SHIPMENT S
       WHERE S.SHIPMENT_GID IN
             (SELECT R.SHIPMENT_GID
                FROM SHIPMENT_REFNUM R
               WHERE R.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.PARADA_MULTIPLA'
                 AND R.SHIPMENT_REFNUM_VALUE = 'SIM'
                 AND R.SHIPMENT_GID = l_shipment
              UNION
              SELECT R1.SHIPMENT_GID
                FROM SHIPMENT_REFNUM R1
               WHERE R1.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.TIPO_DE_PEDIDO'
                   AND R1.SHIPMENT_REFNUM_VALUE = 'PARADA MULTIPLA'
                   AND R1.SHIPMENT_GID = l_shipment )
         AND S.SHIPMENT_GID = l_shipment;
    EXCEPTION
        WHEN OTHERS THEN
          l_parada_multipla := 'N';
    END;

    IF l_parada_multipla = 'N' THEN

      BEGIN
        SELECT DECODE(p_min_ou_max,
                      'MIN',
                      to_char(MIN(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)), 'DD/MM/YYYY HH24:MI:SS'),
                      'MAX',
                      to_char(MAX(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)  ), 'DD/MM/YYYY HH24:MI:SS'),
                      NULL)
          INTO l_data_evento
          FROM SS_STATUS_HISTORY SSH,
               IE_SHIPMENTSTATUS IES,
               SHIPMENT_REFNUM   SHIP_UNIT
         WHERE SSH.I_TRANSACTION_NO               = IES.I_TRANSACTION_NO
           AND IES.STATUS_CODE_GID                = p_evento
           AND SSH.SHIPMENT_GID                   = SHIP_UNIT.SHIPMENT_GID
           AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
           AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
           AND SSH.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                      FROM SHIPMENT_REFNUM SHIP_UNIT2,
                                           SHIPMENT_STATUS COND_SHIP
                                     WHERE COND_SHIP.STATUS_VALUE_GID          = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                       AND COND_SHIP.STATUS_TYPE_GID           = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                       AND SHIP_UNIT2.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                       AND SHIP_UNIT2.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                       AND SHIP_UNIT2.SHIPMENT_REFNUM_VALUE    = SHIP_UNIT.SHIPMENT_REFNUM_VALUE)
           -- Considerar o ultimo evento inserido
           AND IES.INSERT_DATE IN (SELECT DECODE(p_min_ou_max,
                                         'MIN', MIN(IE.INSERT_DATE),
                                         'MAX', MAX(IE.INSERT_DATE))
                                   FROM SS_STATUS_HISTORY SSSH,
                                        IE_SHIPMENTSTATUS IE,
                                        SHIPMENT_REFNUM   SHIP_UNIT3
                                   WHERE SSSH.I_TRANSACTION_NO             = IE.I_TRANSACTION_NO
                                   AND IE.STATUS_CODE_GID                  = p_evento
                                   AND SSSH.SHIPMENT_GID                   = SHIP_UNIT3.SHIPMENT_GID
                                   AND SHIP_UNIT3.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                   AND SHIP_UNIT3.SHIPMENT_REFNUM_VALUE    = SHIP_UNIT.SHIPMENT_REFNUM_VALUE
                                   ); --Select incluido por FSEnna em 07/07/16

        /*SELECT DECODE(p_min_ou_max,
                      'MIN',
                      --to_char(MIN(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS'),
                      to_char(MIN(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)), 'DD/MM/YYYY HH24:MI:SS'),
                      'MAX',
                      --to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS'),
                      to_char(MAX(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)  ), 'DD/MM/YYYY HH24:MI:SS'),
                      NULL)
          INTO l_data_evento
          FROM SS_STATUS_HISTORY SSH,
               IE_SHIPMENTSTATUS IES
         WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
           AND IES.STATUS_CODE_GID  = p_evento
           AND SSH.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                      FROM SHIPMENT_REFNUM SHIP_UNIT,
                                           SHIPMENT_STATUS COND_SHIP
                                     WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                       AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                       AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                       AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                       AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid)
           -- Considerar o ultimo evento inserido
           AND IES.INSERT_DATE IN ( SELECT DECODE(p_min_ou_max,
                                          'MIN', MIN(IE.INSERT_DATE),
                                          'MAX', MAX(IE.INSERT_DATE))
                                      FROM SS_STATUS_HISTORY SSSH,
                                           IE_SHIPMENTSTATUS IE
                                     WHERE SSSH.I_TRANSACTION_NO = IE.I_TRANSACTION_NO
                                       AND IE.STATUS_CODE_GID = p_evento
                                       AND SSSH.SHIPMENT_GID = SSH.SHIPMENT_GID);*/ --Select retirado por FSEnna em 07/07/16
      EXCEPTION
        WHEN OTHERS THEN
          l_data_evento := ' ';
      END;

    ELSE
    -- Caso seja Parada Multipla
      BEGIN
      SELECT DECODE(p_min_ou_max,
                    'MIN',
                    --to_char(MIN(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS'),
                    to_char(MIN(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)), 'DD/MM/YYYY HH24:MI:SS'),
                    'MAX',
                    --to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS'),
                    to_char(MAX(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)  ), 'DD/MM/YYYY HH24:MI:SS'),
                    NULL)
        INTO l_data_evento
        FROM SS_STATUS_HISTORY SSH,
             IE_SHIPMENTSTATUS IES
       WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
         AND IES.STATUS_CODE_GID  = p_evento
         AND SSH.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                    FROM SHIPMENT_REFNUM SHIP_UNIT,
                                         SHIPMENT_STATUS COND_SHIP
                                   WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                     AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                     AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                     AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                     AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid);
      EXCEPTION
      WHEN OTHERS THEN
        l_data_evento := ' ';
      END;

      END IF;


    --
    RETURN l_data_evento;
    --
  END recupera_data_evento;
  --
  FUNCTION recupera_navio_viagem_transb(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_navio_viagem_transbordo          varchar2(100);
  --
  BEGIN
    BEGIN
      FOR c IN  (SELECT VIAG_TRNB.VOYAGE_XID VIAGEM_TRANSB
                   FROM SHIPMENT_REFNUM SHIP_UNIT,
                        SHIPMENT        SHIP_TRNB,
                        SHIPMENT_STATUS COND_SHIP,
                        VOYAGE           VIAG_TRNB
                  WHERE SHIP_TRNB.VOYAGE_GID               =  VIAG_TRNB.VOYAGE_GID
                    AND COND_SHIP.STATUS_VALUE_GID         IN (SHIP_TRNB.DOMAIN_NAME || '.CONDICAO_SHIPMENT_VALIDO',  SHIP_TRNB.DOMAIN_NAME || '.CONDICAO_SHIPMENT_ERRO_RECAL_DATAS')
                    AND COND_SHIP.STATUS_TYPE_GID          =  SHIP_TRNB.DOMAIN_NAME || '.INT_CONDICAO_SHIPMENT'
                    AND SHIP_TRNB.SHIPMENT_GID             =  COND_SHIP.SHIPMENT_GID
                    AND SHIP_TRNB.IS_PRIMARY               =  'N'
                    AND SHIP_TRNB.TRANSPORT_MODE_GID       =  'VESSEL-CH'
                    AND SHIP_UNIT.SHIPMENT_GID             =  SHIP_TRNB.SHIPMENT_GID
                    AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID =  'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                    AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    =  p_ship_unit_xid ) LOOP
          --
          IF l_navio_viagem_transbordo IS NULL THEN
            l_navio_viagem_transbordo := C.VIAGEM_TRANSB;
          ELSE
            l_navio_viagem_transbordo := l_navio_viagem_transbordo||'/'||C.VIAGEM_TRANSB;
          END IF;
          --
        END LOOP;
        --
      EXCEPTION
        WHEN OTHERS THEN
          l_navio_viagem_transbordo := ' ';
      END;
      --
      RETURN l_navio_viagem_transbordo;
      --
  END recupera_navio_viagem_transb;
  --
  FUNCTION recupera_location_xid(p_location_gid in location.location_gid%TYPE) RETURN VARCHAR2 AS

    l_location_xid          varchar2(100);
  --
  BEGIN
    BEGIN
      SELECT LOCATION_XID
        INTO l_location_xid
        FROM LOCATION
       WHERE LOCATION_GID = p_location_gid;
    EXCEPTION
       WHEN OTHERS THEN
          l_location_xid := ' ';
    END;
    --
    RETURN l_location_xid;
    --
  END recupera_location_xid;
  --
  FUNCTION recupera_ISO_container(p_EquimentReferenteUnitGid IN equip_group_equip_ref_unit.equipment_reference_unit_gid%type,
                                  p_EquimentGroupGid         IN equip_group_equip_ref_unit.equipment_group_gid%TYPE)  RETURN VARCHAR2 is
    --
    l_ISO equipment_type.iso_equipment_code%type;
    --
    begin
      BEGIN
        SELECT ET.ISO_EQUIPMENT_CODE
          INTO l_ISO
          FROM EQUIP_GROUP_EQUIP_REF_UNIT EG,
               EQUIPMENT_TYPE_JOIN        EJ,
               EQUIPMENT_TYPE             ET
         WHERE ET.EQUIPMENT_TYPE_GID           = EJ.EQUIPMENT_TYPE_GID
           AND EJ.EQUIPMENT_GROUP_GID          = EG.EQUIPMENT_GROUP_GID
           AND EG.EQUIPMENT_GROUP_GID          = p_EquimentGroupGid
           AND EG.EQUIPMENT_REFERENCE_UNIT_GID = p_EquimentReferenteUnitGid;
      EXCEPTION
         WHEN OTHERS THEN
           l_ISO := ' ';
    END;
    --
    RETURN  l_ISO;
  --
  END recupera_ISO_container;
  --
  FUNCTION recupera_ck_reefer_container(p_EquimentReferenteUnitGid IN equip_group_equip_ref_unit.equipment_reference_unit_gid%type,
                                        p_EquimentGroupGid         IN equip_group_equip_ref_unit.equipment_group_gid%TYPE)  RETURN VARCHAR2 is
    --
    l_ck_Reefer varchar2(1);
    --
    begin
      BEGIN
        SELECT DECODE(instr(et.equipment_type_name, 'REEFER'),0,'N','S')
          INTO l_ck_Reefer
          FROM EQUIP_GROUP_EQUIP_REF_UNIT EG,
               EQUIPMENT_TYPE_JOIN        EJ,
               EQUIPMENT_TYPE             ET
         WHERE ET.EQUIPMENT_TYPE_GID           = EJ.EQUIPMENT_TYPE_GID
           AND EJ.EQUIPMENT_GROUP_GID          = EG.EQUIPMENT_GROUP_GID
           AND EG.EQUIPMENT_GROUP_GID          = p_EquimentGroupGid
           AND EG.EQUIPMENT_REFERENCE_UNIT_GID = p_EquimentReferenteUnitGid;
      EXCEPTION
         WHEN OTHERS THEN
           l_ck_Reefer := ' ';
    END;
    --
    RETURN   l_ck_Reefer;
  --
  END recupera_ck_reefer_container;
  --
  FUNCTION recupera_ship_group_refnum(p_ship_group_gid  IN ship_group_refnum.ship_group_gid%TYPE,
                                      p_refnum_qual_gid IN ship_group_refnum.ship_group_refnum_qual_gid%TYPE)
           RETURN VARCHAR2 AS

    is_ship_group ship_group_refnum.ship_group_refnum_value%TYPE;

  BEGIN
    BEGIN
      SELECT MAX(ship_group_refnum_value)
        INTO is_ship_group
        FROM ship_group_refnum
       WHERE ship_group_refnum_qual_gid = p_refnum_qual_gid
         AND ship_group_gid = p_ship_group_gid;
    EXCEPTION
      WHEN OTHERS THEN
        is_ship_group := ' ';
    END;

    RETURN is_ship_group;

  END recupera_ship_group_refnum;
  --
  FUNCTION recupera_IMO  (p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    L_IMO          HAZMAT_GENERIC.HAZMAT_GENERIC_XID%type;
  --
  BEGIN
    BEGIN
      SELECT NVL(HZM.HAZMAT_GENERIC_XID, ' ') IMO
        INTO L_IMO
        FROM SHIP_UNIT          SUN,
             SHIP_UNIT_LINE     SUL,
             ORDER_RELEASE_LINE RLN,
              HAZMAT_GENERIC     HZM
         WHERE HZM.HAZMAT_GENERIC_GID (+) = RLN.HAZ_HAZMAT_GENERIC_GID
           AND RLN.ORDER_RELEASE_LINE_GID = SUL.ORDER_RELEASE_LINE_GID
           AND SUL.SHIP_UNIT_GID = SUN.SHIP_UNIT_GID
           AND SUN.SHIP_UNIT_XID = p_ship_unit_xid;
    EXCEPTION
      WHEN OTHERS THEN
        L_IMO := ' ';
    END;
    --
    RETURN  L_IMO;
  --
  END recupera_IMO;
  --
  /*FUNCTION recupera_cidade_origem_trecho(p_ship_unit_xid   in ship_unit.ship_unit_xid%TYPE,
                                         p_class_trecho    in leg_classification.leg_classification_xid%type) RETURN VARCHAR2 AS

    l_cidade  varchar2(500);
  --
  BEGIN
    BEGIN
      FOR c IN  (SELECT LOC.CITY||'-'||LOC.PROVINCE_CODE CIDADE
                   FROM SHIPMENT_REFNUM  CLAS_TREC,
                        SHIPMENT         SHIP,
                        LOCATION         LOC
                  WHERE LOC.LOCATION_GID  = SHIP.SOURCE_LOCATION_GID
                    AND SHIP.SHIPMENT_GID = CLAS_TREC.SHIPMENT_GID
                    AND CLAS_TREC.SHIPMENT_REFNUM_VALUE    = p_class_trecho
                    AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                    AND CLAS_TREC.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                                     FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                          SHIPMENT_STATUS COND_SHIP
                                                    WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                      AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                      AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                                      AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                      AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid) ) LOOP
          --
          IF l_cidade IS NULL THEN
            l_cidade := c.cidade;
          ELSE
            l_cidade := l_cidade||'/'||c.cidade;
          END IF;
          --
        END LOOP;
        --
      EXCEPTION
        WHEN OTHERS THEN
          l_cidade := ' ';
      END;
      --
      RETURN l_cidade;
      --
  END recupera_cidade_origem_trecho;*/

  -- Altera? Karina 29/04/2014
  FUNCTION recupera_cidade_origem_trecho(p_ship_unit_xid   in ship_unit.ship_unit_xid%TYPE,
                                         p_class_trecho    in leg_classification.leg_classification_xid%type) RETURN VARCHAR2 AS

    l_cidade  varchar2(500);
  --
  BEGIN
    BEGIN
      For w in  (SELECT SHIP_UNIT.SHIPMENT_GID
                   FROM SHIPMENT_REFNUM SHIP_UNIT,
                        SHIPMENT_STATUS COND_SHIP
                  WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                    AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                    AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                    AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                    AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid) LOOP
          FOR c IN  (SELECT LOC.CITY||'-'||LOC.PROVINCE_CODE CIDADE
                       FROM SHIPMENT_REFNUM  CLAS_TREC,
                            SHIPMENT         SHIP,
                            LOCATION         LOC
                       WHERE LOC.LOCATION_GID  = SHIP.SOURCE_LOCATION_GID
                         AND SHIP.SHIPMENT_GID = CLAS_TREC.SHIPMENT_GID
                         AND CLAS_TREC.SHIPMENT_REFNUM_VALUE    = p_class_trecho
                         AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                         AND CLAS_TREC.SHIPMENT_GID  = w.shipment_gid ) LOOP
             Begin
              --
               IF l_cidade IS NULL THEN
                  l_cidade := c.cidade;
               ELSE
                  l_cidade := l_cidade||'/'||c.cidade;
               END IF;
             END;
              --
          END LOOP;
      END LOOP;
        --
      EXCEPTION
        WHEN OTHERS THEN
          l_cidade := ' ';
      END;
      --
      RETURN l_cidade;
      --
  END recupera_cidade_origem_trecho;

FUNCTION recupera_cidade_destino_trecho(p_ship_unit_xid   in ship_unit.ship_unit_xid%TYPE,
                                         p_class_trecho    in leg_classification.leg_classification_xid%type) RETURN VARCHAR2 AS

    l_cidade  varchar2(500);
  --
  BEGIN
    BEGIN
      FOR c IN  (/*SELECT LOC.CITY||'-'||LOC.PROVINCE_CODE CIDADE
                   FROM SHIPMENT_REFNUM  CLAS_TREC,
                        SHIPMENT         SHIP,
                        LOCATION         LOC
                  WHERE LOC.LOCATION_GID  = SHIP.DEST_LOCATION_GID
                    AND SHIP.SHIPMENT_GID = CLAS_TREC.SHIPMENT_GID
                    AND CLAS_TREC.SHIPMENT_REFNUM_VALUE    = p_class_trecho
                    AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                    AND CLAS_TREC.SHIPMENT_GID IN */
                   select LOC.CITY||'-'||LOC.PROVINCE_CODE CIDADE
                     from shipment           shp,
                          itinerary          itn,
                          itinerary_detail   idt,
                          leg                leg,
                          leg_classification lgc,
                          location           loc
                    where loc.location_gid           = shp.dest_location_gid
                      and lgc.leg_classification_gid = leg.leg_classification_gid
                      and leg.leg_gid                = idt.leg_gid
                      and idt.leg_gid                = shp.parent_leg_gid
                      and idt.itinerary_gid          = itn.itinerary_gid
                      and itn.itinerary_gid          = shp.itinerary_gid
                      and lgc.leg_classification_xid = p_class_trecho
                      and shp.shipment_gid IN     (SELECT SHIP_UNIT.SHIPMENT_GID
                                                     FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                          SHIPMENT_STATUS COND_SHIP
                                                    WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                      AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                      AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                                      AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                      AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid) ) LOOP
          --
          IF l_cidade IS NULL THEN
            l_cidade := c.cidade;
          ELSE
            l_cidade := l_cidade||'/'||c.cidade;
          END IF;
          --
        END LOOP;
        --
      EXCEPTION
        WHEN OTHERS THEN
          l_cidade := ' ';
      END;
      --
      RETURN l_cidade;
      --
  END recupera_cidade_destino_trecho;
  --
  FUNCTION recupera_destinatario_ctms(p_shipment              IN shipment_remark.shipment_gid%TYPE,
                                      p_cnpj_razao_social  IN varchar2)
           RETURN VARCHAR2 AS

    l_retorno     varchar2(1000);
    l_remark_text varchar2(1000);

  BEGIN
    BEGIN
        --
        IF p_cnpj_razao_social = 'CNPJ' then
          --
          for c in (SELECT DISTINCT REPLACE(REPLACE(REPLACE(To_Char(LPad(sr_cnpj.remark_text,14 ,'0')

                             ,'00,000,000,0000,00') ,',' ,'.')  ,' ')  ,'.'||Trim(To_Char(Trunc(Mod(LPad(sr_cnpj.remark_text

                             ,14 ,'0') ,1000000)/100) ,'0000'))||'.'

                             ,'/'||Trim(To_Char(Trunc(Mod(LPad(sr_cnpj.remark_text,14 ,'0') ,1000000)/100) ,'0000'))||'-') cnpj
                  FROM shipment_remark sr_cnpj
                 WHERE sr_cnpj.remark_qual_gid = 'LOGPUBLICO.CNPJ_DESTINATARIO'
                   AND sr_cnpj.shipment_gid = p_shipment) loop
             --
             IF l_retorno IS NOT NULL THEN
             --
             l_retorno := l_retorno||'#'||c.cnpj;
             --
             ELSE
              --
              l_retorno := c.cnpj;
              --
             END IF;
             --
          end loop;
          --
        ELSIF p_cnpj_razao_social = 'RAZAO_SOCIAL' then
          -- Alterado em 19/02/2015 por Orleans
          -- Melhora na performace.
          --
          Begin
             Select LPAD(sr_cnpj.remark_text, 15, '0')
               into l_remark_text
               from shipment_remark sr_cnpj
              where sr_cnpj.remark_qual_gid   = 'LOGPUBLICO.CNPJ_DESTINATARIO'
                and sr_cnpj.shipment_gid      = p_shipment;
             Exception when others then l_remark_text := 'nada';
          End;
          if l_remark_text <> 'nada' then
             for c in ( SELECT
                               distinct (Select lc_razs.location_name
                                           from location        lc_razs
                                          where lc_razs.location_gid = lr_cnpj.location_gid) razao_social
                          FROM location_refnum lr_cnpj
                              ,location_refnum lr_ativ
                         WHERE lr_cnpj.location_refnum_qual_gid = 'LOGPUBLICO.CPF_CNPJ_OUTROS'
                           AND lr_cnpj.location_refnum_value    = l_remark_text
                           AND lr_cnpj.location_gid             = lr_ativ.location_gid
                           AND lr_ativ.location_refnum_qual_gid = 'LOGPUBLICO.ATIVO'
                           AND lr_ativ.location_refnum_value    = 'S') loop
                   IF l_retorno IS NOT NULL THEN
                      l_retorno := l_retorno||'#'||c.razao_social;
                   ELSE
                      l_retorno := c.razao_social;
                   END IF;
             end loop;
          end if;
        END IF;
        --
    EXCEPTION
      WHEN OTHERS THEN
        l_retorno := ' ';
    END;

    RETURN l_retorno;

  END recupera_destinatario_ctms;
  --
  FUNCTION recupera_shipment_transbordo(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                        p_seq_transbordo     IN number)
           RETURN VARCHAR2 AS

    l_shipment_gid shipment.shipment_gid%type;
    l_cont number := 0;

  BEGIN
    BEGIN
      for c in (SELECT SHIP_UNIT.SHIPMENT_GID
                  FROM SHIPMENT_REFNUM    SHIP_UNIT,
                       SHIPMENT_STATUS    COND_SHIP,
                       SHIPMENT_REFNUM    CLAS_TREC,
                       SHIPMENT           SHIP_TRAN
                 WHERE SHIP_TRAN.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID
                   AND CLAS_TREC.SHIPMENT_REFNUM_VALUE    = 'TRANSBORDO'
                   AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                   AND CLAS_TREC.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID
                   AND COND_SHIP.STATUS_VALUE_GID         IN (SHIP_UNIT.DOMAIN_NAME || '.CONDICAO_SHIPMENT_VALIDO',  SHIP_UNIT.DOMAIN_NAME || '.CONDICAO_SHIPMENT_ERRO_RECAL_DATAS')
                   AND COND_SHIP.STATUS_TYPE_GID          =  SHIP_UNIT.DOMAIN_NAME || '.INT_CONDICAO_SHIPMENT'
                   AND COND_SHIP.SHIPMENT_GID             =  SHIP_UNIT.SHIPMENT_GID
                   AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                   AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                 ORDER BY SHIP_TRAN.START_TIME) loop
        --
        l_cont := l_cont + 1;

        IF p_seq_transbordo = l_cont THEN
          --
          l_shipment_gid := c.shipment_gid;
          --
        END IF;
        --
      end loop;
      --
    EXCEPTION
      WHEN OTHERS THEN
        l_shipment_gid := 'SEM_RETORNO';
    END;

    RETURN nvl(l_shipment_gid,'SEM_RETORNO');
    --
  END recupera_shipment_transbordo;

  FUNCTION recupera_data_program_shipment(p_shipment_gid           in shipment.shipment_gid%TYPE,
                                         p_arrival_or_departure   in varchar,
                                         p_stop_inicial_ou_final  in varchar2) RETURN VARCHAR2 AS
  --
    l_dt_program_arriv_stop_ini         varchar2(100);
    l_dt_program_arriv_stop_fin         varchar2(100);
    l_dt_program_depart_stop_ini        varchar2(100);
    l_dt_program_depart_stop_fin        varchar2(100);
    l_dt_programada                     varchar2(100);
  --
  BEGIN
    BEGIN
      SELECT (to_char(MIN(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_ARRIVAL, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')),
             (to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_ARRIVAL, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')),
             (to_char(MIN(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_DEPARTURE, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')),
             (to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_DEPARTURE, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS'))
        INTO l_dt_program_arriv_stop_ini,  -- primeiro stop
             l_dt_program_arriv_stop_fin,  -- ultimo stop
             l_dt_program_depart_stop_ini,-- primeiro stop
             l_dt_program_depart_stop_fin -- ultimo stop
        FROM SHIPMENT_STOP    SHIP_STOP,
             LOCATION         LOCA_STOP
       WHERE SHIP_STOP.LOCATION_GID = LOCA_STOP.LOCATION_GID
         AND SHIP_STOP.SHIPMENT_GID = p_shipment_gid;
      EXCEPTION
        WHEN OTHERS THEN
          l_dt_programada := ' ';
      END;
      --
      IF p_stop_inicial_ou_final = 'I' THEN
        --
        IF p_arrival_or_departure = 'A' THEN
           l_dt_programada := l_dt_program_arriv_stop_ini;
        ELSIF p_arrival_or_departure = 'D' THEN
           l_dt_programada := l_dt_program_depart_stop_ini;
        ELSE
          l_dt_programada := ' ';
        END IF;
        --
      ELSIF  p_stop_inicial_ou_final = 'F' THEN
        --
        IF p_arrival_or_departure = 'A' THEN
           l_dt_programada := l_dt_program_arriv_stop_fin;
        ELSIF p_arrival_or_departure = 'D' THEN
           l_dt_programada := l_dt_program_depart_stop_fin;
        ELSE
          l_dt_programada := ' ';
        END IF;
        --
      END IF;
      --
      RETURN l_dt_programada;
      --
  END recupera_data_program_shipment;
  --
  FUNCTION recupera_data_evento_shipment(p_shipmentt_gid      in shipment.shipment_gid%TYPE,
                                         p_evento             in IE_SHIPMENTSTATUS.STATUS_CODE_GID%type,
                                         p_min_ou_max         in varchar2) RETURN VARCHAR2 AS
  --
    l_data_evento          varchar2(100);
  --
  BEGIN
    BEGIN
      SELECT DECODE(p_min_ou_max,
                    'MIN',
                    to_char(MIN(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)), 'DD/MM/YYYY HH24:MI:SS'),
                    'MAX',
                    to_char(MAX(NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID), IES.EVENTDATE)  ), 'DD/MM/YYYY HH24:MI:SS'),
                    NULL)
        INTO l_data_evento
        FROM SS_STATUS_HISTORY SSH,
             IE_SHIPMENTSTATUS IES
       WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
         AND IES.STATUS_CODE_GID  = p_evento
         AND SSH.SHIPMENT_GID     = p_shipmentt_gid
         -- os 96796
         AND IES.INSERT_DATE IN ( SELECT DECODE(p_min_ou_max,
                                          'MIN', MIN(IE.INSERT_DATE),
                                          'MAX', MAX(IE.INSERT_DATE))
                                      FROM SS_STATUS_HISTORY SSSH,
                                           IE_SHIPMENTSTATUS IE
                                     WHERE SSSH.I_TRANSACTION_NO = IE.I_TRANSACTION_NO
                                       AND IE.STATUS_CODE_GID = p_evento
                                       AND SSSH.SHIPMENT_GID = SSH.SHIPMENT_GID);
    EXCEPTION
      WHEN OTHERS THEN
        l_data_evento := ' ';
    END;
    --
    RETURN l_data_evento;
    --
  END recupera_data_evento_shipment;
  --
  FUNCTION recupera_navio_viagem_shipment(p_shipment_gid in shipment.shipment_gid%TYPE) RETURN VARCHAR2 AS
    --
    l_navio_viagem          varchar2(100);
    --
  BEGIN
    BEGIN
      SELECT VIAG.VOYAGE_XID NAVIO_VIAGEM
        INTO l_navio_viagem
        FROM SHIPMENT        SHIP,
             VOYAGE          VIAG
       WHERE VIAG.VOYAGE_GID   = SHIP.VOYAGE_GID
         AND SHIP.SHIPMENT_GID =  p_shipment_gid;
    EXCEPTION
        WHEN OTHERS THEN
          l_navio_viagem := ' ';
    END;
      --
      RETURN l_navio_viagem;
      --
  END recupera_navio_viagem_shipment;
  --
  FUNCTION recupera_dt_program_fim_viagem(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS
  --
    l_dt_program_fim_viagem         varchar2(100);
  --
  BEGIN
    BEGIN
      L_DT_PROGRAM_FIM_VIAGEM := LLI_OTM_UTIL_PKG.GET_DT_program_fim_viagem ( p_ship_unit_xid , null );

      EXCEPTION WHEN OTHERS THEN l_dt_program_fim_viagem := '';
    END;

    IF l_dt_program_fim_viagem is null then
       BEGIN
         SELECT to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(SHIP_STOP.PLANNED_ARRIVAL, 'GMT0',LOCA_STOP.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')
           INTO l_dt_program_fim_viagem
           FROM SHIPMENT         SHIP_MARI,
                SHIPMENT_STOP    SHIP_STOP,
                LOCATION         LOCA_STOP
          WHERE LOCA_STOP.LOCATION_GID        = SHIP_STOP.LOCATION_GID
            AND SHIP_STOP.SHIPMENT_GID        = SHIP_MARI.SHIPMENT_GID
            AND SHIP_MARI.TRANSPORT_MODE_GID  = 'VESSEL-CH'
            AND SHIP_MARI.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                             FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                  SHIPMENT_STATUS COND_SHIP
                                            WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                              AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                              AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                              AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                              AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid);
         EXCEPTION WHEN OTHERS THEN l_dt_program_fim_viagem := ' ';
       END;
    END IF;

    RETURN l_dt_program_fim_viagem;

  END recupera_dt_program_fim_viagem;
  --
  FUNCTION recupera_dt_saida_nav_schedul(p_shipment_gid in shipment.shipment_gid%TYPE) RETURN VARCHAR2 AS
    --
    l_dt_saida_navio         varchar2(100);
    --
  BEGIN
    BEGIN
      select to_char(lli_inv_envia_otm_inv_pkg.GET_TIME(sgp.start_time ,'GMT0',LOC.TIME_ZONE_GID), 'dd/mm/yyyy hh24:mi:ss')
        into l_dt_saida_navio
        from ship_group        sgp,
             ship_group_d      sgd,
             ship_group_refnum sgv,
             shipment          shi,
             voyage            voy,
             location          loc
       where loc.location_gid            = sgp.source_location_gid
         and sgv.ship_group_refnum_value = voy.voyage_xid
         and sgv.ship_group_gid          = sgp.ship_group_gid
         and voy.voyage_gid              = shi.voyage_gid
         and shi.shipment_gid            = sgd.shipment_gid
         and sgp.ship_group_gid          = sgd.ship_group_gid
         and sgp.ship_group_type_gid     = 'LOGPLAN.CAPACIDADE'
         and sgd.shipment_gid            = p_shipment_gid;
        --
      EXCEPTION
        WHEN OTHERS THEN
          l_dt_saida_navio := ' ';
      END;
      --
      RETURN l_dt_saida_navio;
      --
  END recupera_dt_saida_nav_schedul;
  --
  FUNCTION recupera_status_ship_unit_BIC(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_status                       varchar2(100);
    l_status_code_gid              bs_status_code.bs_status_code_gid%type;
    l_domain_name                  shipment.domain_name%type;
    l_shipment_gid                 shipment.shipment_gid%type;
   -- l_status_tender_entrega        status_value.status_value_gid%type;
   -- l_status_tender_coleta         status_value.status_value_gid%type;
    l_seq_leg_mar_principal        itinerary_detail.sequence_no%type;
    l_status_tender_maritimo_princ status_value.status_value_gid%type;
    l_rem_maritima_com_POD         shipment.shipment_gid%type;
    l_class_trecho                 varchar2(101);
    l_status_agendamento_entrega        status_value.status_value_gid%type;
    l_status_agendamento_coleta         status_value.status_value_gid%type;
  --
  BEGIN
    BEGIN
      SELECT STATUS_CODE_GID,
             DOMAIN_NAME,
             SHIPMENT_GID
        INTO l_status_code_gid,
             l_domain_name,
             l_shipment_gid
        FROM ( SELECT --to_char(
                      NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID),IES.EVENTDATE),-- 'DD/MM/YYYY HH24:MI:SS'),
                      IES.STATUS_CODE_GID,
                      IES.DOMAIN_NAME,
                      SSH.SHIPMENT_GID
                 FROM SS_STATUS_HISTORY SSH,
                      IE_SHIPMENTSTATUS IES
                WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
                  AND IES.STATUS_CODE_GID IN ('LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO',
                                              'LOGPUBLICO.CHEGADA_NAVIO_COM_ATRASO',
                                              'LOGPUBLICO.SAIDA_NAVIO_PORTO_ORIGEM',
                                              'LOGPUBLICO.SAIDA_NAVIO_COM_ATRASO',
                                              'LOGPUBLICO.DVS',
                                              'LOGPUBLICO.RMI',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_DEPOT_VAZIOS_ENTREGA',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.CHEGADA_DEPOT_VAZIOS_ENTREGA',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_DESTINATARIO',
                                              'LOGPUBLICO.CHEGADA_DESTINATARIO',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_TERMINAL_DESEMBARQUE',
                                              'LOGPUBLICO.DVC',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.CHEGADA_TERMINAL_DESEMBARQUE',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.DCF',
                                              'LOGPUBLICO.LDF',
                                              'LOGPUBLICO.DCFT',
                                              'LOGPUBLICO.LDFT',
                                              'LOGPUBLICO.SAIDA_TERMINAL_EMBARQUE',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.RFE',
                                              'LOGPUBLICO.CHEGADA_TERMINAL_EMBARQUE',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_EMBARCADOR',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.CHEGADA_EMBARCADOR')

                  /* OS 165676 - Ricardo - 22/06/2016 */
                  and   (IES.insert_date, ies.status_code_gid)  in  (select max (IES_L.insert_date) insert_date, IES_L.status_code_gid
                            from IE_SHIPMENTSTATUS IES_L, SS_STATUS_HISTORY SSH_L
                            where SSH_L.shipment_gid = SSH.SHIPMENT_GID
                            and SSH_L.I_TRANSACTION_NO = IES_L.I_TRANSACTION_NO
                            group by IES_L.status_code_gid )
                  /* FIM OS 165676 - Ricardo - 22/06/2016 */

                  AND SSH.SHIPMENT_GID IN /*(SELECT SHIP_UNIT.SHIPMENT_GID
                                             FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                  SHIPMENT_STATUS COND_SHIP
                                             WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                               AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                               AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                               AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                               AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                               )*/
                                               --
                                               -- 24/10/2014 - Regiane Piza - OS 96796
                                               -- Desconsiderar os eventos do trecho RODOVIARIA que n?o seja rodo puro.
                                              (SELECT SHIP_UNIT.SHIPMENT_GID
                                                 FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                      SHIPMENT_STATUS COND_SHIP,
                                                      SHIPMENT             SHIP,
                                                      ITINERARY            ITNR,
                                                      ITINERARY_DETAIL     ITND,
                                                      SHIPMENT_REFNUM      CLTR
                                                WHERE (    CLTR.SHIPMENT_REFNUM_VALUE   = 'RODOVIARIA' AND ITND.SEQUENCE_NO = 1
                                                           OR
                                                           CLTR.SHIPMENT_REFNUM_VALUE   <> 'RODOVIARIA'
                                                      )
                                                  AND      CLTR.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                                                  AND      CLTR.SHIPMENT_GID             = SHIP.SHIPMENT_GID
                                                  AND      ITND.LEG_GID                  = SHIP.PARENT_LEG_GID
                                                  AND      ITND.ITINERARY_GID            = ITNR.ITINERARY_GID
                                                  AND      ITNR.ITINERARY_GID            = SHIP.ITINERARY_GID
                                                  AND      SHIP.PERSPECTIVE              = 'B'
                                                  AND      SHIP.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID
                                                  AND COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                  AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                  AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                                  AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                  AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                               )
          ORDER BY 1 DESC)
        WHERE ROWNUM <= 1;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        BEGIN
          SELECT SU.DOMAIN_NAME
            INTO l_domain_name
            FROM SHIPMENT_REFNUM SU
           WHERE SU.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
             AND SU.SHIPMENT_REFNUM_VALUE = p_ship_unit_xid
             AND ROWNUM <= 1;
        EXCEPTION
          WHEN OTHERS THEN
            l_status := ' ';
            return l_status;
           END;
      WHEN OTHERS THEN
        l_status := ' ';
        return l_status;
    END;
    -- Se tem evento
    IF l_status_code_gid IS NOT NULL THEN
      --
                      IF l_status_code_gid in ('LOGPUBLICO.RMI',
                                               'LOGPUBLICO.SAIDA_DEPOT_VAZIOS_ENTREGA'--Regiane Piza 12/05/2014
                                              ) THEN
                        --
                        l_status := 'UNIDADE VAZIA DEVOLVIDA NO DEPOT';
                        --
                      ELSIF l_status_code_gid in ('LOGPUBLICO.SAIDA_DESTINATARIO',
                                                  'LOGPUBLICO.CHEGADA_DEPOT_VAZIOS_ENTREGA'--Regiane Piza 12/05/2014
                                                 ) THEN
                        --
                        l_status := 'ENTREGA REALIZADA';
                        --
                      ELSIF l_status_code_gid in ('LOGPUBLICO.DVC',--Regiane Piza 12/05/2014
                                                  'LOGPUBLICO.SAIDA_TERMINAL_DESEMBARQUE',
                                                  'LOGPUBLICO.CHEGADA_DESTINATARIO'--Regiane Piza 12/05/2014
                                                  ) THEN
                        --
                       -- l_status_tender_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', l_domain_name||'.INT_TENDER');
                        --
                        l_status_agendamento_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', l_domain_name||'.AGENDAMENTO_RODOVIARIO');

                        --IF l_status_tender_entrega  IS NULL THEN --Regiane Piza 16/05/2014
                       IF l_status_agendamento_entrega  IS NULL THEN --Ricardo

                          --
                          l_status := 'UNIDADE RETIRADA DO PORTO'; --Regiane Piza 16/05/2014
                          --
                        ELSE
                          l_status := 'ENTREGA EM ANDAMENTO';
                        END IF;
                        --
                      ELSIF l_status_code_gid in ('LOGPUBLICO.DCF',
                                                  'LOGPUBLICO.CHEGADA_TERMINAL_DESEMBARQUE'--Regiane Piza 12/05/2014
                                                 ) THEN
                        --
                       -- l_status_tender_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', l_domain_name||'.INT_TENDER');
                        l_status_agendamento_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', l_domain_name||'.AGENDAMENTO_RODOVIARIO');

                        --
                       -- IF l_status_tender_entrega  IS NULL THEN --Regiane Piza 12/05/2014
                        IF l_status_agendamento_entrega  IS NULL THEN --Ricardo
                          --
                          l_status := 'AGUARDANDO RETIRADA DO PORTO'; --Regiane Piza 12/05/2014
                          --
                        --ELSIF  l_status_tender_entrega = 'TENDER_ACEITO' THEN
                        ELSIF  (l_status_agendamento_entrega = 'DISPONIVEL_AGR' OR l_status_agendamento_entrega = 'DISPONIVEL_MANUAL_AGR')  THEN
                                                  --
                          l_status := 'ENTREGA PROGRAMADA';
                          --
                        --ELSIF (l_status_tender_entrega = 'TENDER_ENVIADO' OR l_status_tender_entrega = 'TENDER_ENVIO_NAO_INICIADO') THEN
                        ELSIF (l_status_agendamento_entrega = 'NAO_AGENDADO_AGR' OR
                                l_status_agendamento_entrega = 'DATA_FORA_DO_PERMITIDO_AGR' OR
                                   l_status_agendamento_entrega = 'ERRO_VALIDACAO_AGR' OR
                                      l_status_agendamento_entrega = 'INDISPONIVEL_AGR') then
                          --
                          l_status := 'AGUARDANDO PROGRAMACAO DE ENTREGA';
                          --
                        END IF;
                        --
                      ELSIF l_status_code_gid IN ('LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO',
                                                  'LOGPUBLICO.CHEGADA_NAVIO_COM_ATRASO') THEN
                        --
                        l_rem_maritima_com_POD := lli_otm_report_funcoes_pkg.recupera_rem_maritima_com_pod(p_ship_unit_xid);
                        --
                        IF l_status_code_gid = 'LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO' THEN
                          IF l_rem_maritima_com_POD = l_shipment_gid THEN
                            l_status := 'AGUARDANDO O DESEMBARQUE DA UNIDADE';-- Somente se for o porto de destino final
                          ELSE
                            l_status := 'EM TRANSITO MARITIMO';
                          END IF;
                        END IF;
                        --
                        IF l_status_code_gid = 'LOGPUBLICO.CHEGADA_NAVIO_COM_ATRASO' THEN
                          IF l_rem_maritima_com_POD = l_shipment_gid THEN
                            l_status := 'CHEGADA_NAVIO_COM_ATRASO';-- Somente se for o porto de destino final
                          ELSE
                            l_status := 'EM TRANSITO MARITIMO';
                          END IF;
                        END IF;
                        --
                      ELSIF l_status_code_gid IN ('LOGPUBLICO.LDF','LOGPUBLICO.LDFT') THEN
                        --
                        l_status := 'UNIDADE EMBARCADA';--'EM TRANSITO MARITIMO';
                        --
                      ELSIF l_status_code_gid IN ('LOGPUBLICO.DCFT') THEN
                        --
                        l_status := 'AGUARDANDO O TRANSBORDO';
                        --
                      ELSIF l_status_code_gid IN ('LOGPUBLICO.SAIDA_NAVIO_PORTO_ORIGEM',
                                                  'LOGPUBLICO.SAIDA_NAVIO_COM_ATRASO') THEN
                         --
                         l_status := 'EM TRANSITO MARITIMO';
                         --
                       ELSIF l_status_code_gid IN ('LOGPUBLICO.RFE',
                                                  'LOGPUBLICO.SAIDA_TERMINAL_EMBARQUE'--Regiane Piza 12/05/2014
                                                  ) THEN
                        --
                        l_status := 'AGUARDANDO EMBARQUE';
                        --
                      ELSIF l_status_code_gid IN ('LOGPUBLICO.CHEGADA_EMBARCADOR') THEN
                        --
                        l_status := 'COLETA EM ANDAMENTO';
                        --
                      ELSIF l_status_code_gid IN (/*'LOGPUBLICO.DVS',*/ -- Altera??o por Karina Campos em 24/11/2015 (OS 143937)
                                                  'LOGPUBLICO.SAIDA_EMBARCADOR',
                                                  'LOGPUBLICO.CHEGADA_TERMINAL_EMBARQUE'
                                                  ) THEN
                        --
                        l_status := 'AGUARDANDO DEPOSITO NO PORTO';
                        --
                      -- #1 Altera??o por Karina Campos em 24/11/2015 (OS 143937)
                      ELSIF l_status_code_gid IN ('LOGPUBLICO.DVS') THEN
                        --
                        l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(l_shipment_gid, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');
                        IF l_class_trecho = 'COLETA_MARITIMA' THEN
                          l_status := 'COLETA PROGRAMADA';
                        ELSE
                          l_status := 'AGUARDANDO DEPOSITO NO PORTO';
                        END IF;
                        --
                      -- #1
                      ELSE
                        --
                              /* Comentado por Ricardo em 29/03/16
                              l_status_tender_coleta := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'COLETA_MARITIMA', l_domain_name||'.INT_TENDER');
                              --
                              IF l_status_tender_coleta = 'TENDER_ACEITO' THEN
                                --
                                l_status := 'COLETA PROGRAMADA';
                                --
                              ELSIF (l_status_tender_coleta = 'TENDER_ENVIADO' OR l_status_tender_coleta = 'TENDER_ENVIO_NAO_INICIADO') THEN
                                --
                                l_status := 'AGUARDANDO PROGRAMACAO DE COLETA';
                                --
                             ELSE */
                          l_status_agendamento_coleta := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'COLETA_MARITIMA', l_domain_name||'.AGENDAMENTO_RODOVIARIO');

                          IF (l_status_agendamento_coleta = 'DISPONIVEL_AGR' OR l_status_agendamento_coleta = 'DISPONIVEL_MANUAL_AGR') THEN
                                             --
                                l_status := 'COLETA PROGRAMADA';

                          ELSIF (l_status_agendamento_coleta = 'NAO_AGENDADO_AGR' OR
                                        l_status_agendamento_coleta = 'DATA_FORA_DO_PERMITIDO_AGR' OR
                                           l_status_agendamento_coleta = 'ERRO_VALIDACAO_AGR' OR
                                              l_status_agendamento_coleta = 'INDISPONIVEL_AGR') THEN
                                --
                               l_status := 'AGUARDANDO PROGRAMACAO DE COLETA';
                                --
                          ELSE
                              l_status := ' ';
                          END IF;
                      END IF;
                      --
   ELSE  -- Caso n?o tenha evento
              --
             -- l_status_tender_coleta := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'COLETA_MARITIMA', l_domain_name||'.INT_TENDER');
              l_status_agendamento_coleta := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'COLETA_MARITIMA', l_domain_name||'.AGENDAMENTO_RODOVIARIO');

              --
              --IF l_status_tender_coleta = 'TENDER_ACEITO' THEN
              IF (l_status_agendamento_coleta = 'DISPONIVEL_AGR' OR l_status_agendamento_coleta = 'DISPONIVEL_MANUAL_AGR') THEN
                             --
                l_status := 'COLETA PROGRAMADA';
                --
             -- ELSIF (l_status_tender_coleta = 'TENDER_ENVIADO' OR l_status_tender_coleta = 'TENDER_ENVIO_NAO_INICIADO') THEN
              ELSIF (l_status_agendamento_coleta = 'NAO_AGENDADO_AGR' OR
                        l_status_agendamento_coleta = 'DATA_FORA_DO_PERMITIDO_AGR' OR
                           l_status_agendamento_coleta = 'ERRO_VALIDACAO_AGR' OR
                              l_status_agendamento_coleta = 'INDISPONIVEL_AGR') THEN
                --
                l_status := 'AGUARDANDO PROGRAMACAO DE COLETA';
                --
              ELSE
              --
                          l_status := ' ';
                          --
                          BEGIN
                            select itd.sequence_no,
                                   lli_otm_report_funcoes_pkg.recupera_shipment_status(shp.shipment_gid, l_domain_name||'.INT_TENDER')
                              into l_seq_leg_mar_principal,
                                   l_status_tender_maritimo_princ
                              From shipment         shp,
                                   itinerary_detail itd,
                                   leg              leg
                             where itd.leg_gid      = shp.parent_leg_gid
                               and leg.leg_gid      =  itd.leg_gid
                               and itd.itinerary_gid = shp.itinerary_gid
                               and shp.shipment_gid = lli_otm_report_funcoes_pkg.recupera_shipment_vessel_princ(p_ship_unit_xid);
                          EXCEPTION
                            WHEN OTHERS THEN
                              l_status := ' ';
                          END;
                          --
                          if l_seq_leg_mar_principal = 1 then
                            --
                                if l_status_tender_maritimo_princ IN ('TENDER_ENVIO_NAO_INICIADO','TENDER_ENVIADO','TENDER_ACEITO') then
                                  --
                                  l_status := 'AGUARDANDO RETIRADA DO VAZIO';
                                  --
                                else
                                  l_status := ' ';
                                end if;
                                --
                           else
                                l_status := ' ';
                           end if;
                       --
                 END IF;
      --
    END IF; -- Caso n?o tenha evento
      --
    RETURN l_status;
    --
  END recupera_status_ship_unit_BIC;
  --
  FUNCTION recupera_shipment_vessel_princ(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE)

           RETURN VARCHAR2 AS

    l_shipment_gid shipment.shipment_gid%type;

  BEGIN
    BEGIN
      SELECT SHIP_UNIT.SHIPMENT_GID
        INTO l_shipment_gid
        FROM SHIPMENT           SHIP_VESS,
             SHIPMENT_REFNUM    SHIP_UNIT,
             SHIPMENT_STATUS    COND_SHIP
       WHERE SHIP_VESS.TRANSPORT_MODE_GID       = 'VESSEL-CH'
         AND SHIP_VESS.IS_PRIMARY               = 'Y'
         AND SHIP_VESS.PERSPECTIVE              = 'B'
         AND SHIP_VESS.SHIPMENT_GID             =  SHIP_UNIT.SHIPMENT_GID
         AND COND_SHIP.STATUS_VALUE_GID         IN (SHIP_UNIT.DOMAIN_NAME || '.CONDICAO_SHIPMENT_VALIDO',  SHIP_UNIT.DOMAIN_NAME || '.CONDICAO_SHIPMENT_ERRO_RECAL_DATAS')
         AND COND_SHIP.STATUS_TYPE_GID          =  SHIP_UNIT.DOMAIN_NAME || '.INT_CONDICAO_SHIPMENT'
         AND COND_SHIP.SHIPMENT_GID             =  SHIP_UNIT.SHIPMENT_GID
         AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
         AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid;
         --
    EXCEPTION
      WHEN OTHERS THEN
        l_shipment_gid := 'SEM_RETORNO';
    END;

    RETURN nvl(l_shipment_gid,'SEM_RETORNO');
    --
  END recupera_shipment_vessel_princ;
  --
  FUNCTION recupera_location_refnum(p_location        IN location_refnum.location_gid%TYPE,
                                    p_refnum_qual_gid IN location_refnum.location_refnum_qual_gid%TYPE)
           RETURN VARCHAR2 AS

    is_location location_refnum.location_refnum_value%TYPE;

  BEGIN
    BEGIN
      SELECT MAX(location_refnum_value)
        INTO is_location
        FROM location_refnum
       WHERE location_refnum_qual_gid = p_refnum_qual_gid
         AND location_gid = p_location;
    EXCEPTION
      WHEN OTHERS THEN
        is_location := ' ';
    END;

    RETURN is_location;

  END recupera_location_refnum;
  --
  FUNCTION recupera_rem_maritima_com_POD(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_shipment_gid shipment.shipment_gid%type;

  BEGIN
    BEGIN
      select shipment_gid
        into l_shipment_gid
        from
            (select sp.shipment_gid
               from shipment_refnum su,
                    shipment        sp,
                    shipment_status ss
              where ss.status_value_gid = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                and ss.status_type_gid = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                and ss.shipment_gid = sp.shipment_gid
                and sp.perspective = 'B'
                and sp.transport_mode_gid = 'VESSEL-CH'
                and sp.shipment_gid = su.shipment_gid
                and su.shipment_refnum_qual_gid = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                and su.shipment_refnum_value = p_ship_unit_xid
           order by sp.end_time desc)
           where rownum <= 1;
    EXCEPTION
      WHEN OTHERS THEN
        l_shipment_gid := ' ';
    END;

    RETURN l_shipment_gid;

  END recupera_rem_maritima_com_POD;
  --
  FUNCTION recupera_data_evento_trecho(p_class_trecho       in shipment_refnum.shipment_refnum_value%TYPE,
                                       p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                       p_evento             in IE_SHIPMENTSTATUS.STATUS_CODE_GID%type,
                                       p_min_ou_max         in varchar2) RETURN VARCHAR2 AS
  --
    l_data_evento          varchar2(100);
    l_shipment             SHIPMENT.SHIPMENT_GID%TYPE;
    --
  BEGIN
    --
    begin
      SELECT SP.SHIPMENT_GID
        INTO l_shipment
        FROM SHIPMENT        SP,
             SHIPMENT_REFNUM SU,
             SHIPMENT_STATUS SS,
             SHIPMENT_REFNUM CT
       WHERE CT.SHIPMENT_REFNUM_VALUE    = p_class_trecho
         AND CT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
         AND CT.SHIPMENT_GID             = SP.SHIPMENT_GID
         AND SS.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
         AND SS.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
         AND SS.SHIPMENT_GID             = SP.SHIPMENT_GID
         AND SP.PERSPECTIVE              = 'B'
         AND SP.SHIPMENT_GID             = SU.SHIPMENT_GID
         AND SU.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
         AND shipment_type_gid = 'TRANSPORT'                     -- OS 167076 - Ricardo Alves - DT. 11/07/16
         AND SU.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid;

    EXCEPTION WHEN OTHERS THEN
      l_data_evento := ' ';
      RETURN l_data_evento;
    end;
    --
    BEGIN
      l_data_evento := lli_otm_report_funcoes_pkg.recupera_data_evento_shipment(l_shipment,
                                                                                p_evento,
                                                                                p_min_ou_max);
    EXCEPTION WHEN OTHERS THEN
       l_data_evento := ' ';
    END;
    --
    RETURN l_data_evento;
    --
  END recupera_data_evento_trecho;
 --
 FUNCTION rategeo_sell_refnum_shipunit(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE,
                                       p_refnum_qual_gid    in rate_geo_refnum.rate_geo_refnum_qual_gid%type) RETURN VARCHAR2 AS
   --
   l_value  rate_geo_refnum.rate_geo_refnum_value%type;
   --
  BEGIN
    --
    begin
      SELECT RAT_REFN.RATE_GEO_REFNUM_VALUE
        into l_value
        FROM RATE_GEO_REFNUM   RAT_REFN,
             RATE_GEO          RAT_RECD,
             SHIPMENT_REFNUM   SUT_SELL,
             SHIPMENT          SHP_SELL
       WHERE RAT_REFN.RATE_GEO_REFNUM_QUAL_GID  = p_refnum_qual_gid
         AND RAT_REFN.RATE_GEO_GID              = RAT_RECD.RATE_GEO_GID
         AND RAT_RECD.RATE_GEO_GID              = SHP_SELL.RATE_GEO_GID
         AND SHP_SELL.PERSPECTIVE               = 'S'
         AND SHP_SELL.TRANSPORT_MODE_GID        = 'VESSEL-CH'
         AND SHP_SELL.SHIPMENT_GID              = SUT_SELL.SHIPMENT_GID
         AND SUT_SELL.SHIPMENT_REFNUM_QUAL_GID  = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
         AND SUT_SELL.SHIPMENT_REFNUM_VALUE     =  p_ship_unit_xid;
    EXCEPTION WHEN OTHERS THEN
      l_value := ' ';
    end;
    --
    RETURN l_value;
    --
  END rategeo_sell_refnum_shipunit;
  --
  FUNCTION recup_motiv_reprog_tender_rodo(p_ship_unit_xid  in ship_unit.ship_unit_xid%TYPE,
                                          p_class_leg      in leg_classification.leg_classification_xid%TYPE)
            RETURN VARCHAR2 AS

    l_shipment_gid shipment.shipment_gid%type;
    l_motivo_reprogramacao  tender_collab_servprov.decline_reason_code_gid%type;

  BEGIN
    BEGIN
                SELECT SHIP_UNIT.SHIPMENT_GID
                  INTO l_shipment_gid
                  FROM SHIPMENT_REFNUM    SHIP_UNIT,
                       SHIPMENT_STATUS    COND_SHIP,
                       SHIPMENT_REFNUM    CLAS_TREC,
                       SHIPMENT           SHIP_TRAN
                 WHERE SHIP_TRAN.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID
                   AND CLAS_TREC.SHIPMENT_REFNUM_VALUE    = p_class_leg
                   AND CLAS_TREC.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                   AND CLAS_TREC.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID
                   AND COND_SHIP.STATUS_VALUE_GID         IN (SHIP_UNIT.DOMAIN_NAME || '.CONDICAO_SHIPMENT_VALIDO',  SHIP_UNIT.DOMAIN_NAME || '.CONDICAO_SHIPMENT_ERRO_RECAL_DATAS')
                   AND COND_SHIP.STATUS_TYPE_GID          =  SHIP_UNIT.DOMAIN_NAME || '.INT_CONDICAO_SHIPMENT'
                   AND COND_SHIP.SHIPMENT_GID             =  SHIP_UNIT.SHIPMENT_GID
                   AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                   AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid;
    EXCEPTION
      WHEN OTHERS THEN
         null;
    end;
    --
    if l_shipment_gid is not null then
      -- recup_motiv_reprog_tender_rodo
      begin
        select rsc.bs_reason_code_xid
         into  l_motivo_reprogramacao
         From  tender_collaboration        tdc,
               tender_collaboration_status tds,
               tender_collab_servprov      tcs,
               bs_reason_code              rsc
        where  rsc.bs_reason_code_gid      =  tcs.decline_reason_code_gid
          and  tcs.i_transaction_no        = tds.i_transaction_no
          and  tds.i_transaction_no        = tdc.i_transaction_no
          and  tcs.decline_reason_code_gid is not null
          and  tdc.shipment_gid            = l_shipment_gid;
      exception
        when others then
          l_motivo_reprogramacao := ' ';
      end;
    end if;
    --
    return nvl(l_motivo_reprogramacao, ' ');
    --
    END recup_motiv_reprog_tender_rodo;
    --
  FUNCTION recup_quote_cost_option_remark(p_quote_gid        in quote_cost_option_remark.quote_gid%TYPE,
                                          p_cost_option_seq  in quote_cost_option_remark.cost_option_sequence%type,
                                          p_remark_qual_gid  in quote_cost_option_remark.remark_qual_gid%type) RETURN VARCHAR2 AS
   --
   l_value  rate_geo_refnum.rate_geo_refnum_value%type;
   --
  BEGIN
    --
    begin
      SELECT R.REMARK_TEXT
        INTO l_value
        FROM QUOTE_COST_OPTION_REMARK R
       WHERE R.REMARK_QUAL_GID      = p_remark_qual_gid
         AND R.COST_OPTION_SEQUENCE = p_cost_option_seq
         AND R.QUOTE_GID            = p_quote_gid;
    EXCEPTION WHEN OTHERS THEN
      l_value := ' ';
    end;
    --
    RETURN l_value;
    --
  END recup_quote_cost_option_remark;
  --
  FUNCTION recup_shipment_coleta_entrega(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                         p_class_trecho  in shipment_refnum.shipment_refnum_qual_gid%type) RETURN VARCHAR2 AS
   --
   l_value  shipment_refnum.shipment_refnum_value%type;
   --
  BEGIN
    --
    begin
          SELECT MAX(CLT.SHIPMENT_GID) -- OS 183204
             INTO l_value
             FROM SHIPMENT_REFNUM SUN,
                  SHIPMENT_REFNUM CLT,
                  SHIPMENT        SHP,
                  SHIPMENT_STATUS SS
            WHERE SHP.PERSPECTIVE              = 'B'
              AND SHP.SHIPMENT_GID             = CLT.SHIPMENT_GID
              AND SHP.SHIPMENT_GID             = SUN.SHIPMENT_GID
              AND SS.STATUS_VALUE_GID = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO' -- Altera??o por Karina Campos em 26/11/2015 #recuperar shipments validos
              AND SS.STATUS_TYPE_GID = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
              AND SS.SHIPMENT_GID = SHP.SHIPMENT_GID
              AND CLT.SHIPMENT_REFNUM_VALUE    = p_class_trecho
              AND CLT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
              AND CLT.SHIPMENT_GID             = SUN.SHIPMENT_GID
              AND SHP.SHIPMENT_TYPE_GID = 'TRANSPORT' -- OS 183204
              AND SUN.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
              AND SUN.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid;
    EXCEPTION WHEN OTHERS THEN
      l_value := ' ';
    end;
    --
    RETURN l_value;
    --
  END recup_shipment_coleta_entrega;
  --
  FUNCTION recup_dt_status_ship_unit_BIC(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_status                       varchar2(100);
    l_status_code_gid              bs_status_code.bs_status_code_gid%type;
    l_data_status                  varchar2(100);
    l_dt_status                    date;
  --
  BEGIN

    BEGIN
      SELECT STATUS_CODE_GID,
             DATA_STATUS
        INTO l_status_code_gid,
             l_dt_status
        FROM ( SELECT --to_char(
                      NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID),IES.EVENTDATE)/*, 'DD/MM/YYYY HH24:MI:SS')*/ DATA_STATUS,
                      IES.STATUS_CODE_GID,
                      IES.DOMAIN_NAME,
                      SSH.SHIPMENT_GID
                 FROM SS_STATUS_HISTORY SSH,
                      IE_SHIPMENTSTATUS IES
                WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
                  AND IES.STATUS_CODE_GID IN ('LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO',
                                              'LOGPUBLICO.CHEGADA_NAVIO_COM_ATRASO',
                                              'LOGPUBLICO.SAIDA_NAVIO_PORTO_ORIGEM',
                                              'LOGPUBLICO.SAIDA_NAVIO_COM_ATRASO',
                                              'LOGPUBLICO.DVS',
                                              'LOGPUBLICO.RMI',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_DEPOT_VAZIOS_ENTREGA',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.CHEGADA_DEPOT_VAZIOS_ENTREGA',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_DESTINATARIO',
                                              'LOGPUBLICO.CHEGADA_DESTINATARIO',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_TERMINAL_DESEMBARQUE',
                                              'LOGPUBLICO.DVC',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.CHEGADA_TERMINAL_DESEMBARQUE',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.DCF',
                                              'LOGPUBLICO.LDF',
                                              'LOGPUBLICO.DCFT',
                                              'LOGPUBLICO.LDFT',
                                              'LOGPUBLICO.SAIDA_TERMINAL_EMBARQUE',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.RFE',
                                              'LOGPUBLICO.CHEGADA_TERMINAL_EMBARQUE',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.SAIDA_EMBARCADOR',--Regiane Piza 12/05/2014
                                              'LOGPUBLICO.CHEGADA_EMBARCADOR')
                  AND SSH.SHIPMENT_GID IN      -- 24/10/2014 - Regiane Piza - OS 96796
                                               -- Desconsiderar os eventos do trecho RODOVIARIA que n?o seja rodo puro.
                                              (SELECT SHIP_UNIT.SHIPMENT_GID
                                                 FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                      SHIPMENT_STATUS COND_SHIP,
                                                      SHIPMENT             SHIP,
                                                      ITINERARY            ITNR,
                                                      ITINERARY_DETAIL     ITND,
                                                      SHIPMENT_REFNUM      CLTR
                                                WHERE (    CLTR.SHIPMENT_REFNUM_VALUE   = 'RODOVIARIA' AND ITND.SEQUENCE_NO = 1
                                                           OR
                                                           CLTR.SHIPMENT_REFNUM_VALUE   <> 'RODOVIARIA'
                                                      )
                                                  AND      CLTR.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                                                  AND      CLTR.SHIPMENT_GID             = SHIP.SHIPMENT_GID
                                                  AND      ITND.LEG_GID                  = SHIP.PARENT_LEG_GID
                                                  AND      ITND.ITINERARY_GID            = ITNR.ITINERARY_GID
                                                  AND      ITNR.ITINERARY_GID            = SHIP.ITINERARY_GID
                                                  AND      SHIP.PERSPECTIVE              = 'B'
                                                  AND      SHIP.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID
                                                  AND COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                  AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                  AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                                  AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                  AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                               )
          ORDER BY 1 DESC)
        WHERE ROWNUM <= 1;
    EXCEPTION
       WHEN OTHERS THEN
        l_data_status := ' ';
        return l_data_status;
    END;


    IF l_dt_status IS NOT NULL THEN
      l_data_status := to_char(l_dt_status,'DD/MM/YYYY HH24:MI:SS');
      RETURN l_data_status;
    ELSE
      l_data_status := ' ';
      RETURN l_data_status;
    END IF;

  END recup_dt_status_ship_unit_BIC;
  --
  FUNCTION recupera_valor_demurrage (P_quote_Xid                   in quote.quote_Xid%type,
                                     P_quote_cost_option_sequence  in quote_cost_option.cost_option_sequence%type,
                                     P_equipment_group_gid         in quote_cost_option_equipment.equipment_group_gid%type)
                                    RETURN VARCHAR2 as

  L_rate_geo_gid                  varchar2(101);
  L_accessorial_cost_gid          varchar2(101);
  L_valor_demurrage               VARCHAR2(500);

  BEGIN
     L_valor_demurrage := '';

     For reg in (select DISTINCT C.RATE_GEO_GID
                   from rate_geo_refnum c
                  WHERE c.rate_geo_refnum_qual_gid = 'LOGPUBLICO.COTACAO'
                    and c.rate_geo_refnum_value   IN (P_QUOTE_XID,'Gerado com base na Cotacao ' ||P_QUOTE_XID)
                    and exists (select 1 from rate_geo_refnum d
                                 where d.rate_geo_gid = c.rate_geo_gid
                                   and d.rate_geo_refnum_qual_gid = 'LOGPUBLICO.SEQUENCIA'
                                   and d.rate_geo_refnum_value    = P_QUOTE_COST_OPTION_SEQUENCE))


     Loop
        Begin
           SELECT TRIM(p.charge_amount)
             into L_valor_demurrage
             FROM RATE_GEO_ACCESSORIAL  T
                 , accessorial_cost     P
            WHERE T.RATE_GEO_GID         = REG.rate_geo_gid
              AND T.ACCESSORIAL_CODE_GID = 'LOGPUBLICO.DEMURRAGE'
              and P.acceSsorial_cost_gid = T.accessorial_cost_gid
              and P.LOW_VALUE3           = 'CABOTAGEM'
              AND P.LOW_VALUE2           = P_equipment_group_gid
              AND CHARGE_AMOUNT          IS NOT NULL
              AND ROWNUM                 = 1;
           Exception when no_data_found then L_valor_demurrage := null;
                     when others        then L_valor_demurrage := 'Erro3';
        End;
        if l_valor_demurrage IS NOT null then
           exit;
        end if;
     End Loop;

     IF l_VALOR_DEMURRAGE IS NULL THEN
        RETURN (P_quote_Xid || ' - ' || P_quote_cost_option_sequence || ' - ' || P_equipment_group_gid);
     ELSE
        RETURN (L_valor_demurrage);
     END IF;
  END recupera_valor_demurrage;

  FUNCTION recupera_NotaFiscal_PRODUTO(P_shipment_gid                in shipment.shipment_gid%type)
                                RETURN VARCHAR2 as


  l_nota_fiscal varchar2(4000);
  l_shipunit    varchar2(101);

  BEGIN

     l_shipunit    := lli_otm_report_funcoes_pkg.recupera_shipment_refnum (P_shipment_gid,'LOGPUBLICO.SHIP_UNIT_PEDIDO');
     l_nota_fiscal := null;

     For reg in (select k.numero_nf||'-'|| k.serie_nf notafiscal
                   from lli_otm_shipunit_notafiscal k
                  where k.ship_unit_gid  = 'LOGPLAN.' || l_shipunit
                  order by 1)
     Loop

        if l_nota_fiscal is null then
           l_nota_fiscal := reg.notafiscal;
        else
           l_nota_fiscal := l_nota_fiscal || '/' || reg.notafiscal;
        end if;

     End loop;

     if l_nota_fiscal is null then
        l_nota_fiscal := replace(replace(replace(SUBSTR(LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REMARK(P_SHIPMENT_GID, 'LOGPUBLICO.NOTA_FISCAL_PRODUTO'), 1, 2000),';',','), '#','/'),'-EX','');
     end if;

     Return (l_nota_fiscal);
  END recupera_NotaFiscal_PRODUTO;


  FUNCTION recupera_Valor_NFiscal_PRODUTO(P_shipment_gid                in shipment.shipment_gid%type)
                                          RETURN VARCHAR2 as


  l_valor_nota_fiscal varchar2(4000);
  l_shipunit          varchar2(101);

  BEGIN

     l_shipunit          := lli_otm_report_funcoes_pkg.recupera_shipment_refnum (P_shipment_gid,'LOGPUBLICO.SHIP_UNIT_PEDIDO');
     l_valor_nota_fiscal := null;

     For reg in (select k.numero_nf||'-'|| k.serie_nf notafiscal, k.valor
                   from lli_otm_shipunit_notafiscal k
                  where k.ship_unit_gid  = 'LOGPLAN.' || l_shipunit
                  order by 1)
     Loop

        if l_valor_nota_fiscal is null then
           l_valor_nota_fiscal := to_char(reg.valor);
        else
           l_valor_nota_fiscal := l_valor_nota_fiscal || '/' || to_char(reg.valor);
        end if;

     End loop;

     Return (l_valor_nota_fiscal);

  END recupera_Valor_NFiscal_PRODUTO;

  FUNCTION recupera_data_ETB(p_ShipmentGiddMarPrincipal  shipment.shipment_gid%type) return varchar2 as
    --
    V_SOURCE_LOCATION_GID     SHIPMENT.SOURCE_LOCATION_GID%TYPE;
    V_VOYAGE_GID              VOYAGE.VOYAGE_GID%TYPE;
    V_DATA_ETB                VARCHAR2(18);
    --
  BEGIN
    --
    BEGIN
      SELECT SOURCE_LOCATION_GID,
             VOYAGE_GID
        INTO V_SOURCE_LOCATION_GID,
             V_VOYAGE_GID
        FROM SHIPMENT
       WHERE SHIPMENT_GID = p_ShipmentGiddMarPrincipal;
    EXCEPTION
      WHEN OTHERS THEN
        RETURN 'Erro 1';
    END;
    --
    BEGIN
      SELECT TO_CHAR(lli_inv_envia_otm_inv_pkg.GET_TIME(dt_etb,'GMT0',time_zone_gid),'dd/mm/rrrr hh24:mi') dt_etd
        INTO V_DATA_ETB
        FROM (SELECT MAX(vl.d_or_a_date) dt_etb,
                     lc.time_zone_gid
                FROM voyloc vl,
                     location lc
               WHERE vl.location_gid   = lc.location_gid
                 AND vl.voyage_gid     = V_VOYAGE_GID
                 AND vl.location_gid   = V_SOURCE_LOCATION_GID
                 AND vl.d_or_a         = 'A'
                 AND vl.d_or_a_date    < (SELECT g.START_TIME
                                            FROM ship_group   g,
                                                 ship_group_d d
                                           WHERE d.ship_group_gid = g.ship_group_gid
                                             AND d.shipment_gid     = p_ShipmentGiddMarPrincipal
                                             AND SHIP_GROUP_TYPE_GID = 'LOGPLAN.CAPACIDADE')    -- Ricardo -- Dt. 23/11/2016 -- OS 179633
               GROUP BY lc.time_zone_gid);
          --
    EXCEPTION
      WHEN OTHERS THEN
        RETURN 'Erro 2';
    END;
    --
    RETURN V_DATA_ETB;
    --
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 'Erro 3';
    --
  END;
  --
  FUNCTION recupera_equipment_type_remark(p_equipment_type_gid IN glogowner.equipment_type_remark.equipment_type_gid%TYPE,
                                          p_remark_qual_gid    IN glogowner.equipment_type_remark.remark_qual_gid%TYPE)
           RETURN VARCHAR2 AS

    is_valor glogowner.equipment_type_remark.remark_text%TYPE;

  BEGIN
    BEGIN
      SELECT remark_text
        INTO is_valor
        FROM glogowner.equipment_type_remark
       WHERE remark_qual_gid = p_remark_qual_gid
         AND equipment_type_gid = p_equipment_type_gid;
    EXCEPTION
      WHEN OTHERS THEN
        is_valor := ' ';
    END;

    RETURN is_valor;

  END recupera_equipment_type_remark;
  --
  FUNCTION codigo_proposta_comercial(p_ship_unit_xid      in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS
   --
   l_value  rate_offering.rate_version_gid%type;
   --
  BEGIN
    --
    begin
      SELECT REPLACE(RAT_OFFE.RATE_VERSION_GID,'LOGPLAN.','')
        into l_value
        FROM RATE_OFFERING     RAT_OFFE,
             SHIPMENT_REFNUM   SUT_SELL,
             SHIPMENT          SHP_SELL
       WHERE RAT_OFFE.RATE_OFFERING_GID         = SHP_SELL.RATE_OFFERING_GID
         AND SHP_SELL.PERSPECTIVE               = 'S'
         AND SHP_SELL.TRANSPORT_MODE_GID        = 'VESSEL-CH'
         AND SHP_SELL.SHIPMENT_GID              = SUT_SELL.SHIPMENT_GID
         AND SUT_SELL.SHIPMENT_REFNUM_QUAL_GID  = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
         AND SUT_SELL.SHIPMENT_REFNUM_VALUE     = p_ship_unit_xid;
    EXCEPTION WHEN OTHERS THEN
      l_value := ' ';
    end;
    --
    RETURN l_value;
    --
  END codigo_proposta_comercial;
  --
  -- Inclus?o por Karina Campos em 24/11/2015 (OS 143937)
  FUNCTION recupera_data_status_ship_unit(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE) RETURN VARCHAR2 AS

    l_status                       varchar2(100);
    l_status_code_gid              bs_status_code.bs_status_code_gid%type;
    l_domain_name                  shipment.domain_name%type;
    l_shipment_gid                 shipment.shipment_gid%type;
    l_status_tender_entrega        status_value.status_value_gid%type;
    l_status_tender_coleta         status_value.status_value_gid%type;
    l_seq_leg_mar_principal        itinerary_detail.sequence_no%type;
    l_status_tender_maritimo_princ status_value.status_value_gid%type;
    l_rem_maritima_com_POD         shipment.shipment_gid%type;
    l_class_trecho                 varchar2(101);
    l_data_status                  varchar2(100);
    l_dt_status                    date;
    l_ship_entrega                 shipment.shipment_gid%type;
    l_ship_coleta                  shipment.shipment_gid%type;
    l_remessa                      shipment.shipment_gid%type;
  --
  BEGIN
    BEGIN
      SELECT STATUS_CODE_GID,
             DOMAIN_NAME,
             SHIPMENT_GID,
             DATA_STATUS
        INTO l_status_code_gid,
             l_domain_name,
             l_shipment_gid,
             l_dt_status
        FROM ( SELECT --to_char(
                      NVL(lli_inv_envia_otm_inv_pkg.GET_TIME(IES.EVENTDATE, 'GMT0',IES.TIME_ZONE_GID),IES.EVENTDATE)/*, 'DD/MM/YYYY HH24:MI:SS')*/ DATA_STATUS,
                      IES.STATUS_CODE_GID,
                      IES.DOMAIN_NAME,
                      SSH.SHIPMENT_GID
                 FROM SS_STATUS_HISTORY SSH,
                      IE_SHIPMENTSTATUS IES
                WHERE SSH.I_TRANSACTION_NO = IES.I_TRANSACTION_NO
                  AND IES.STATUS_CODE_GID IN ('LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO',
                                              'LOGPUBLICO.CHEGADA_NAVIO_COM_ATRASO',
                                              'LOGPUBLICO.SAIDA_NAVIO_PORTO_ORIGEM',
                                              'LOGPUBLICO.SAIDA_NAVIO_COM_ATRASO',
                                              'LOGPUBLICO.DVS',
                                              'LOGPUBLICO.RMI',
                                              'LOGPUBLICO.SAIDA_DEPOT_VAZIOS_ENTREGA',
                                              'LOGPUBLICO.CHEGADA_DEPOT_VAZIOS_ENTREGA',
                                              'LOGPUBLICO.SAIDA_DESTINATARIO',
                                              'LOGPUBLICO.CHEGADA_DESTINATARIO',
                                              'LOGPUBLICO.SAIDA_TERMINAL_DESEMBARQUE',
                                              'LOGPUBLICO.DVC',
                                              'LOGPUBLICO.CHEGADA_TERMINAL_DESEMBARQUE',
                                              'LOGPUBLICO.DCF',
                                              'LOGPUBLICO.LDF',
                                              'LOGPUBLICO.DCFT',
                                              'LOGPUBLICO.LDFT',
                                              'LOGPUBLICO.SAIDA_TERMINAL_EMBARQUE',
                                              'LOGPUBLICO.RFE',
                                              'LOGPUBLICO.CHEGADA_TERMINAL_EMBARQUE',
                                              'LOGPUBLICO.SAIDA_EMBARCADOR',
                                              'LOGPUBLICO.CHEGADA_EMBARCADOR')
                  AND SSH.SHIPMENT_GID IN -- Desconsiderar os eventos do trecho RODOVIARIA que n?o seja rodo puro.
                                              (SELECT SHIP_UNIT.SHIPMENT_GID
                                                 FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                      SHIPMENT_STATUS COND_SHIP,
                                                      SHIPMENT             SHIP,
                                                      ITINERARY            ITNR,
                                                      ITINERARY_DETAIL     ITND,
                                                      SHIPMENT_REFNUM      CLTR
                                                WHERE (    CLTR.SHIPMENT_REFNUM_VALUE   = 'RODOVIARIA' AND ITND.SEQUENCE_NO = 1
                                                           OR
                                                           CLTR.SHIPMENT_REFNUM_VALUE   <> 'RODOVIARIA'
                                                      )
                                                  AND      CLTR.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                                                  AND      CLTR.SHIPMENT_GID             = SHIP.SHIPMENT_GID
                                                  AND      ITND.LEG_GID                  = SHIP.PARENT_LEG_GID
                                                  AND      ITND.ITINERARY_GID            = ITNR.ITINERARY_GID
                                                  AND      ITNR.ITINERARY_GID            = SHIP.ITINERARY_GID
                                                  AND      SHIP.PERSPECTIVE              = 'B'
                                                  AND      SHIP.SHIPMENT_GID             = SHIP_UNIT.SHIPMENT_GID
                                                  AND COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                  AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                  AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                                  AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                  AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                               )
          ORDER BY 1 DESC)
        WHERE ROWNUM <= 1;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        BEGIN
          SELECT SU.DOMAIN_NAME
            INTO l_domain_name
            FROM SHIPMENT_REFNUM SU
           WHERE SU.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
             AND SU.SHIPMENT_REFNUM_VALUE = p_ship_unit_xid
             AND ROWNUM <= 1;
        EXCEPTION
          WHEN OTHERS THEN
            l_data_status := ' ';
            return l_data_status;
           END;
      WHEN OTHERS THEN
        l_data_status := ' ';
        return l_data_status;
    END;

    l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(l_shipment_gid, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');

    IF l_dt_status IS NOT NULL THEN
      l_data_status := to_char(l_dt_status,'DD/MM/YYYY HH24:MI:SS');

      IF l_status_code_gid IN ('LOGPUBLICO.LDF',
                               'LOGPUBLICO.LDFT',
                               'LOGPUBLICO.DCF',
                               'LOGPUBLICO.RMI') THEN

        RETURN l_data_status;

      ELSIF l_status_code_gid IN ('LOGPUBLICO.SAIDA_DESTINATARIO',
                                  'LOGPUBLICO.SAIDA_DEPOT_VAZIOS_ENTREGA') THEN
        IF l_class_trecho = 'ENTREGA_MARITIMA' THEN
          return l_data_status;
        ELSE
          l_data_status := ' ';
          return l_data_status;
        END IF;

      ELSIF l_status_code_gid IN ('LOGPUBLICO.CHEGADA_TERMINAL_DESEMBARQUE') THEN
        l_status_tender_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', l_domain_name||'.INT_TENDER');
        IF  l_status_tender_entrega = 'TENDER_ACEITO' THEN
          l_data_status := lli_otm_report_funcoes_pkg.recupera_data_program_shipment (l_shipment_gid,'A','I');

          RETURN l_data_status;
        ELSE
          l_data_status := ' ';
          return l_data_status;
        END IF;

      ELSIF l_status_code_gid in ('LOGPUBLICO.DVC') THEN
        l_status_tender_entrega := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'ENTREGA_MARITIMA', l_domain_name||'.INT_TENDER');

        IF l_status_tender_entrega  IS NULL THEN
          RETURN l_data_status;
         ELSE
          -- Data e Hora do campo "Chegada Planejada" contido no primeiro stop da remessa de entrega
         l_ship_entrega := lli_otm_report_funcoes_pkg.recup_shipment_coleta_entrega(p_ship_unit_xid,'ENTREGA_MARITIMA');
         l_data_status := lli_otm_report_funcoes_pkg.recupera_data_program_shipment (l_ship_entrega,'A','I');

         RETURN l_data_status;
        END IF;

      ELSIF l_status_code_gid in ('LOGPUBLICO.CHEGADA_DEPOT_VAZIOS_ENTREGA') THEN
        l_data_status := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_DATA_EVENTO(p_ship_unit_xid, 'LOGPUBLICO.SAIDA_DESTINATARIO', 'MAX');

        RETURN l_data_status;

      ELSIF l_status_code_gid IN ('LOGPUBLICO.DVS',
                                  'LOGPUBLICO.CHEGADA_EMBARCADOR',
                                  'LOGPUBLICO.SAIDA_TERMINAL_DESEMBARQUE',
                                  'LOGPUBLICO.CHEGADA_DESTINATARIO') THEN

        IF l_class_trecho IN ('COLETA_MARITIMA', 'ENTREGA_MARITIMA') THEN
          -- Data e Hora do campo "Chegada Planejada" contido no primeiro stop da remessa de coleta ou entrega
          l_data_status := lli_otm_report_funcoes_pkg.recupera_data_program_shipment (l_shipment_gid,'A','I');

          RETURN l_data_status;
        ELSE
          l_data_status := ' ';
          return l_data_status;
        END IF;

      ELSIF l_status_code_gid IN ('LOGPUBLICO.RFE') THEN

        l_remessa := lli_otm_report_funcoes_pkg.recupera_shipment_vessel_princ(p_ship_unit_xid);
        -- Remessa mar?tima principal.
        if l_remessa is not null then
          l_class_trecho := lli_otm_report_funcoes_pkg.recupera_shipment_refnum(l_remessa,'LOGPUBLICO.CLASSIFICACAO_TRECHO');
          l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POL','ETS',l_class_trecho);
        else
          l_data_status := ' ';
        end if;

       return l_data_status;

      ELSIF l_status_code_gid IN ('LOGPUBLICO.SAIDA_TERMINAL_EMBARQUE') THEN

        IF l_class_trecho = 'COLETA_MARITIMA' THEN

          l_remessa := lli_otm_report_funcoes_pkg.recupera_shipment_vessel_princ(p_ship_unit_xid);
          -- Remessa Mar?tima Principal.
          if l_remessa is not null then
            l_class_trecho := lli_otm_report_funcoes_pkg.recupera_shipment_refnum(l_remessa,'LOGPUBLICO.CLASSIFICACAO_TRECHO');
            l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POL','ETS',l_class_trecho);
          else
            l_data_status := ' ';
          end if;

          return l_data_status;

        END IF;

      ELSIF l_status_code_gid IN ('LOGPUBLICO.SAIDA_NAVIO_PORTO_ORIGEM',
                                   'LOGPUBLICO.SAIDA_NAVIO_COM_ATRASO') THEN

         /*l_remessa := lli_otm_report_funcoes_pkg.recupera_shipment_vessel_princ(p_ship_unit_xid);
          -- Remessa Mar?tima Principal.
          if l_remessa is not null then
            l_class_trecho := lli_otm_report_funcoes_pkg.recupera_shipment_refnum(l_remessa,'LOGPUBLICO.CLASSIFICACAO_TRECHO');
            l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POD', 'ETA',l_class_trecho);
          else
            l_data_status := ' ';
          end if;*/
          -- Remessa maritima que recebeu o evento
          l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POD', 'ETA',l_class_trecho);
          return l_data_status;

      ELSIF l_status_code_gid IN ('LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO',
                                  'LOGPUBLICO.CHEGADA_NAVIO_COM_ATRASO') THEN

        --
        l_rem_maritima_com_POD := lli_otm_report_funcoes_pkg.recupera_rem_maritima_com_pod(p_ship_unit_xid);
        --
        IF l_status_code_gid = 'LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO' THEN
          IF l_rem_maritima_com_POD = l_shipment_gid THEN
            -- l_status := 'AGUARDANDO O DESEMBARQUE DA UNIDADE';-- Somente se for o porto de destino final
            l_data_status := ' ';
            RETURN l_data_status;
          ELSE
            --l_status := 'EM TRANSITO MARITIMO';
            -- Remessa Transbordo.
            l_remessa := lli_otm_report_funcoes_pkg.recupera_shipment_transbordo(p_ship_unit_xid, 1);
            if l_remessa is not null then
              l_class_trecho := lli_otm_report_funcoes_pkg.recupera_shipment_refnum(l_remessa,'LOGPUBLICO.CLASSIFICACAO_TRECHO');
              l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POL','ETS',l_class_trecho);
            else
              l_data_status := ' ';
            end if;
            RETURN l_data_status;
          END IF;
        END IF;
        --
        IF l_status_code_gid = 'LOGPUBLICO.CHEGADA_NAVIO_COM_ATRASO' THEN
          IF l_rem_maritima_com_POD = l_shipment_gid THEN
            -- l_status := 'CHEGADA_NAVIO_COM_ATRASO';-- Somente se for o porto de destino final
            RETURN l_data_status;
          ELSE
            --l_status := 'EM TRANSITO MARITIMO';
            -- Remessa Transbordo.
            l_remessa := lli_otm_report_funcoes_pkg.recupera_shipment_transbordo(p_ship_unit_xid, 1);
            if l_remessa is not null then
              l_class_trecho := lli_otm_report_funcoes_pkg.recupera_shipment_refnum(l_remessa,'LOGPUBLICO.CLASSIFICACAO_TRECHO');
              l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POL','ETS',l_class_trecho);
            else
              l_data_status := ' ';
            end if;
            RETURN l_data_status;
          END IF;
        END IF;

        --
        /*l_rem_maritima_com_POD := lli_otm_report_funcoes_pkg.recupera_rem_maritima_com_pod(p_ship_unit_xid);

        IF l_rem_maritima_com_POD <> l_shipment_gid THEN
          -- Remessa Transbordo.
          l_remessa := lli_otm_report_funcoes_pkg.recupera_shipment_transbordo(p_ship_unit_xid, 1);
          if l_remessa is not null then
            l_class_trecho := lli_otm_report_funcoes_pkg.recupera_shipment_refnum(l_remessa,'LOGPUBLICO.CLASSIFICACAO_TRECHO');
            l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POL','ETS',l_class_trecho);
          else
            l_data_status := ' ';
          end if;
          return l_data_status;
          --
        ELSE
          IF l_status_code_gid = 'LOGPUBLICO.CHEGADA_NAVIO_PORTO_DESTINO' THEN
            l_data_status := ' ';
            return l_data_status;
          ELSE
            return l_data_status;
          END IF;
        END IF;

        return l_data_status; */

      ELSIF l_status_code_gid IN ('LOGPUBLICO.DCFT') THEN

          -- Remessa Transbordo.
          l_remessa := lli_otm_report_funcoes_pkg.recupera_shipment_transbordo(p_ship_unit_xid, 1);
          if l_remessa is not null then
            l_class_trecho := lli_otm_report_funcoes_pkg.recupera_shipment_refnum(l_remessa,'LOGPUBLICO.CLASSIFICACAO_TRECHO');
            l_data_status := lli_otm_report_funcoes_pkg.recupera_dt_viagem_orig_dest(p_ship_unit_xid,'POL','ETS',l_class_trecho);
          else
            l_data_status := ' ';
          end if;
          return l_data_status;
          --

      ELSE
          l_data_status := ' ';
          return l_data_status;
      END IF;

   ELSE

      l_status_tender_coleta := lli_otm_report_funcoes_pkg.recupera_status_class_trecho(p_ship_unit_xid, 'COLETA_MARITIMA', l_domain_name||'.INT_TENDER');

      IF l_status_tender_coleta = 'TENDER_ACEITO' THEN
        l_ship_coleta := lli_otm_report_funcoes_pkg.recup_shipment_coleta_entrega(p_ship_unit_xid,'COLETA_MARITIMA');

        l_class_trecho := LLI_OTM_REPORT_FUNCOES_PKG.RECUPERA_SHIPMENT_REFNUM(l_ship_coleta, 'LOGPUBLICO.CLASSIFICACAO_TRECHO');
        IF l_class_trecho = 'COLETA_MARITIMA' THEN
          -- Data e Hora do campo "Chegada Planejada" contido no primeiro stop da remessa de coleta ou entrega
          l_data_status := lli_otm_report_funcoes_pkg.recupera_data_program_shipment (l_ship_coleta,'A','I');
          RETURN l_data_status;
        ELSE
          l_data_status := ' ';
          return l_data_status;
        END IF;
      ELSE
        l_data_status := ' ';
        return l_data_status;
      END IF;

    END IF;

        IF l_data_status IS NULL THEN
          l_data_status := ' ';
        END IF;

    RETURN l_data_status;
    --
  END recupera_data_status_ship_unit;

  -- Inclus?o por Karina Campos em 24/11/2015 (OS 143937)
  FUNCTION recupera_dt_viagem_orig_dest(p_ship_unit_xid in ship_unit.ship_unit_xid%TYPE,
                                        p_porto IN VARCHAR2,
                                        P_ETS_ETA IN VARCHAR2,
                                        p_class_trecho IN VARCHAR2) RETURN VARCHAR2 AS
  --
    l_dt_viagem         varchar2(100);
    l_ets_eta           varchar2(100);
  --
  BEGIN
    -- 'POL' = Porto de Origem
    -- 'POD' = Porto de Destino

    -- ETS ? Previs?o de sa?da do Navio - Depart
    -- ETA ? Previs?o de chegada do Navio - Arrive

    if P_ETS_ETA = 'ETS' then
      l_ets_eta := 'D';
    else
      l_ets_eta := 'A';
    end if;


    IF p_porto = 'POD' THEN -- porto de destino

    BEGIN
    SELECT to_char(MIN(lli_inv_envia_otm_inv_pkg.GET_TIME(VL.D_OR_A_DATE,'GMT0',L.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')
      INTO l_dt_viagem
      FROM SHIPMENT S
          ,VOYLOC VL
          ,LOCATION L
          ,SHIP_GROUP_D SG
          ,SHIP_GROUP G
     WHERE S.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                FROM SHIPMENT_REFNUM SHIP_UNIT,
                                     SHIPMENT_STATUS COND_SHIP,
                                     SHIPMENT_REFNUM CLT
                               WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                 AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                 AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                 AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                 AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                 AND SHIP_UNIT.SHIPMENT_GID             = CLT.SHIPMENT_GID
                                 AND CLT.SHIPMENT_REFNUM_QUAL_GID       = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                                 AND CLT.SHIPMENT_REFNUM_VALUE          = p_class_trecho)
       AND VL.VOYAGE_GID            = S.VOYAGE_GID
       AND VL.LOCATION_GID          = S.DEST_LOCATION_GID
       AND VL.D_OR_A                = l_ets_eta -- 'A'
       AND L.LOCATION_GID           = VL.LOCATION_GID
       AND SG.SHIPMENT_GID          = S.SHIPMENT_GID
       AND G.SHIP_GROUP_GID         = SG.SHIP_GROUP_GID
       AND G.ship_group_type_gid    = 'LOGPLAN.CAPACIDADE'
       AND G.START_TIME            <= VL.D_OR_A_DATE
       AND VL.D_OR_A_DATE >= ( SELECT  Max(VL1.D_OR_A_DATE)
                      FROM SHIPMENT S1
                          ,VOYLOC VL1
                          ,LOCATION L1
                          ,SHIP_GROUP_D SG1
                          ,SHIP_GROUP G1
                     WHERE S1.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                                          FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                               SHIPMENT_STATUS COND_SHIP,
                                                               SHIPMENT_REFNUM CLT
                                                         WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                           AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                           AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                                           AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                           AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                                           AND SHIP_UNIT.SHIPMENT_GID             = CLT.SHIPMENT_GID
                                                           AND CLT.SHIPMENT_REFNUM_QUAL_GID       = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                                                           AND CLT.SHIPMENT_REFNUM_VALUE          = p_class_trecho)
                       AND VL1.VOYAGE_GID          = S1.VOYAGE_GID
                       AND VL1.LOCATION_GID        = S1.SOURCE_LOCATION_GID
                       AND VL1.D_OR_A              = 'D'
                       AND L1.LOCATION_GID         = VL1.LOCATION_GID
                       AND SG1.SHIPMENT_GID        = S1.SHIPMENT_GID
                       AND G1.SHIP_GROUP_GID       = SG1.SHIP_GROUP_GID
                       AND G1.ship_group_type_gid  = 'LOGPLAN.CAPACIDADE'
                       AND G1.START_TIME           <= VL1.D_OR_A_DATE);
    EXCEPTION
      WHEN OTHERS THEN
        l_dt_viagem := ' ';
        return l_dt_viagem;
    END;

    ELSE

    BEGIN
    SELECT to_char(MAX(lli_inv_envia_otm_inv_pkg.GET_TIME(VL.D_OR_A_DATE,'GMT0',L.TIME_ZONE_GID)), 'DD/MM/YYYY HH24:MI:SS')
      INTO l_dt_viagem
      FROM SHIPMENT S
          ,VOYLOC VL
          ,LOCATION L
          ,SHIP_GROUP_D SG
          ,SHIP_GROUP G
    WHERE S.SHIPMENT_GID  IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                FROM SHIPMENT_REFNUM SHIP_UNIT,
                                     SHIPMENT_STATUS COND_SHIP,
                                     SHIPMENT_REFNUM CLT
                               WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                 AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                 AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                 AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                 AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                 AND SHIP_UNIT.SHIPMENT_GID             = CLT.SHIPMENT_GID
                                 AND CLT.SHIPMENT_REFNUM_QUAL_GID       = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                                 AND CLT.SHIPMENT_REFNUM_VALUE          = p_class_trecho)
       AND VL.VOYAGE_GID            = S.VOYAGE_GID
       AND VL.LOCATION_GID          = S.SOURCE_LOCATION_GID
       AND VL.D_OR_A                = l_ets_eta -- 'A' ou 'D'
       AND L.LOCATION_GID           = VL.LOCATION_GID
       AND SG.SHIPMENT_GID          = S.SHIPMENT_GID
       AND G.SHIP_GROUP_GID         = SG.SHIP_GROUP_GID
       AND G.ship_group_type_gid    = 'LOGPLAN.CAPACIDADE'
       AND G.START_TIME            <= VL.D_OR_A_DATE
       AND VL.D_OR_A_DATE <= ( SELECT MAX(VL1.D_OR_A_DATE)
                                FROM SHIPMENT S1
                                    ,VOYLOC VL1
                                    ,LOCATION L1
                                    ,SHIP_GROUP_D SG1
                                    ,SHIP_GROUP G1
                               WHERE S1.SHIPMENT_GID IN (SELECT SHIP_UNIT.SHIPMENT_GID
                                                                    FROM SHIPMENT_REFNUM SHIP_UNIT,
                                                                         SHIPMENT_STATUS COND_SHIP,
                                                                         SHIPMENT_REFNUM CLT
                                                                   WHERE COND_SHIP.STATUS_VALUE_GID         = 'LOGPLAN.CONDICAO_SHIPMENT_VALIDO'
                                                                     AND COND_SHIP.STATUS_TYPE_GID          = 'LOGPLAN.INT_CONDICAO_SHIPMENT'
                                                                     AND SHIP_UNIT.SHIPMENT_GID             = COND_SHIP.SHIPMENT_GID
                                                                     AND SHIP_UNIT.SHIPMENT_REFNUM_QUAL_GID = 'LOGPUBLICO.SHIP_UNIT_PEDIDO'
                                                                     AND SHIP_UNIT.SHIPMENT_REFNUM_VALUE    = p_ship_unit_xid
                                                                     AND SHIP_UNIT.SHIPMENT_GID             = CLT.SHIPMENT_GID
                                                                     AND CLT.SHIPMENT_REFNUM_QUAL_GID       = 'LOGPUBLICO.CLASSIFICACAO_TRECHO'
                                                                     AND CLT.SHIPMENT_REFNUM_VALUE          = p_class_trecho)
                                 AND VL1.VOYAGE_GID            = S1.VOYAGE_GID
                                 AND VL1.LOCATION_GID          = S1.DEST_LOCATION_GID
                                 AND VL1.D_OR_A                = 'A'
                                 AND L1.LOCATION_GID           = VL1.LOCATION_GID
                                 AND SG1.SHIPMENT_GID          = S1.SHIPMENT_GID
                                 AND G1.SHIP_GROUP_GID         = SG1.SHIP_GROUP_GID
                                 AND G1.ship_group_type_gid    = 'LOGPLAN.CAPACIDADE'
                                 AND G1.START_TIME            <= VL1.D_OR_A_DATE);
    EXCEPTION
      WHEN OTHERS THEN
        l_dt_viagem := ' ';
        return l_dt_viagem;
    END;

    END IF;

    return l_dt_viagem;

  END recupera_dt_viagem_orig_dest;

END LLI_OTM_REPORT_FUNCOES_PKG;