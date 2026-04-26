const pool = require('../database/connection');

//**********************************SERVICIOS PARA SOCIOS***************************************

//////////////TRAER TODA LA LISTA DE SOCIOS //////////////////////////
// app.get('/api/socios', async (req, res) => {
const getSocios = async (req, res) => {
    try {
        // Añadimos ORDER BY idsocio ASC para que siempre sea 01, 02, 03...
        const result = await pool.query('SELECT * FROM socios ORDER BY idsocio ASC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los socios de la base de datos' });
    }
};

//////////////////// REPORTE DE SOCIOS CON CÁLCULOS DE AYUDA, INGRESOS, GASTOS , GANADO Y TOTAL //////////////////////////
// app.get('/api/socios-reporte', async (req, res) => {
const getSociosReporte = async (req, res) => {
    try {
        const query = `
            SELECT 
                s.idsocio, 
                s.nombresocio,
                -- Conteo de aportes por socio
                COALESCE((SELECT COUNT(*) FROM aportes a WHERE a.idsocio = s.idsocio), 0) as num_aportes,
                -- APORTES REALES (Desde la tabla aportes)
                COALESCE((SELECT SUM(a.aportado) FROM aportes a WHERE a.idsocio = s.idsocio), 0) as aportado,

                -- PRÉSTAMOS (Excluyendo Ayudas)
                COALESCE((SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo NOT LIKE 'AYUDA%'), 0) as montoprestado,

                -- INTERÉS PRESTADO
                COALESCE((SELECT SUM(p.interestotal) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo NOT LIKE 'AYUDA%'), 0) as interesprestado,

                -- CAPITAL PAGADO (Pagos reales, no ingresos/egresos/gst)
                COALESCE((SELECT SUM(pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo NOT LIKE 'INGRESO%' AND pa.tipo NOT LIKE 'EGRESO%' AND pa.numamortizacion != 'GST'), 0) as montopagado,

                -- INTERÉS PAGADO
                COALESCE((SELECT SUM(pa.interes) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo NOT LIKE 'INGRESO%' AND pa.tipo NOT LIKE 'EGRESO%' AND pa.numinteres != 'GST'), 0) as interespagado,

                -- AYUDA (Dinámico)
                -- COALESCE((SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo LIKE 'AYUDA%'), 0) as ayuda,

                -- INGRESOS (Dinámico)
                COALESCE((SELECT SUM(pa.interes + pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo LIKE 'INGRESO%'), 0) as ingresos,

                -- GASTOS (Dinámico)
                -- COALESCE((SELECT SUM(pa.interes + pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo LIKE 'EGRESO%'), 0) as gastos,
                COALESCE(
                    (SELECT SUM(pa.interes + pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo LIKE 'EGRESO%'), 
                    0
                ) + 
                COALESCE(
                    (SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo LIKE 'AYUDA%'), 
                    0
                ) -
                 COALESCE(
                    (SELECT SUM(pa.interes) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.numinteres = 'GST'), 
                    0
                ) as gastos,

                -- Mantenemos la columna ayuda por separado si aún la necesitas visualizar sola
                COALESCE(
                    (SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo LIKE 'AYUDA%'), 
                    0
                ) as ayuda,

                -- INTERÉS ANULADO
                COALESCE((SELECT SUM(pa.interes) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.numinteres = 'GST'), 0) as interesanulado

            FROM socios s
            ORDER BY s.idsocio ASC
        `;

        const { rows: socios } = await pool.query(query);

        // --- CÁLCULO DE TOTALES GLOBALES (PARA EL ENCABEZADO) ---
        const totalInteresPrestado = socios.reduce((acc, s) => acc + Number(s.interesprestado), 0);
        const totalIngresos = socios.reduce((acc, s) => acc + Number(s.ingresos), 0);
        const totalInteresAnulado = socios.reduce((acc, s) => acc + Number(s.interesanulado), 0);
        const totalGastos = socios.reduce((acc, s) => acc + Number(s.gastos), 0);

        // LOGS DE ENCABEZADOS
        console.log('--- VALORES GLOBALES RECUPERADOS ---');
        console.log(`(+) Total Interés Prestado: ${totalInteresPrestado}`);
        console.log(`(+) Total Ingresos:         ${totalIngresos}`);
        console.log(`(-) Total Interés Anulado:  ${totalInteresAnulado}`);
        console.log(`(-) Total Gastos (Egr+Ayu): ${totalGastos}`);

        // --- CÁLCULO DE UTILIDAD NETA ---
        const utilidadNeta = (totalInteresPrestado + totalIngresos) - (totalInteresAnulado + totalGastos);

        // --- DINAMISMO DE SOCIOS ---
        // Filtramos para excluir cuentas administrativas (01, 02) si no reciben utilidades
        const sociosQueRecibenUtilidad = socios.filter(s => s.idsocio !== '01' && s.idsocio !== '02');
        const cantidadSociosReales = sociosQueRecibenUtilidad.length;

        // LOG DE DINAMISMO
        console.log(`--- DIVISIÓN DINÁMICA ---`);
        console.log(`Socios que participan:      ${cantidadSociosReales}`);

        // Calculamos el ganado por cabeza basado en la cantidad actual
        const ganadoIndividual = cantidadSociosReales > 0 ? (utilidadNeta / cantidadSociosReales) : 0;
        console.log(`(DIV) Ganado por socio:     ${ganadoIndividual.toFixed(2)}`);
        console.log('------------------------------------');

        const respuesta = socios.map(s => {
            const esEspecial = (s.idsocio === '01' || s.idsocio === '02');
            const miGanado = esEspecial ? 0 : ganadoIndividual;

            // Calculamos saldos pendientes aquí mismo para que siempre sean exactos
            const montoPendiente = Number(s.montoprestado) - Number(s.montopagado);
            const interesPendiente = Number(s.interesprestado) - (Number(s.interespagado) + Number(s.interesanulado));

            return {
                ...s,
                montopendiente: Number(montoPendiente.toFixed(2)),
                interespendiente: Number(interesPendiente.toFixed(2)),
                ganado: Number(miGanado.toFixed(2)),
                total: Number((Number(s.aportado) + miGanado).toFixed(2))
            };
        });

        res.json(respuesta);
    } catch (err) {
        console.error("Error en reporte:", err.message);
        res.status(500).send("Error en el servidor");
    }
};

//////////////////////GENERAR EL SIGUIENTE ID PARA NUEVO SOCIO////////////////////////
// app.get('/api/socios/siguiente-id', async (req, res) => {
const getSiguienteIdSocio = async (req, res) => {
    try {
        // 1. Obtenemos el máximo convirtiendo el string a entero en SQL
        // CAST asegura que '10' sea mayor que '09' numéricamente
        const query = 'SELECT MAX(CAST(idsocio AS INTEGER)) as max_id FROM socios';
        const result = await pool.query(query);

        const ultimoId = result.rows[0].max_id || 0;
        const siguienteNumero = ultimoId + 1;

        // 2. Formatear con ceros a la izquierda (Padding)
        // .padStart(2, '0') convierte 1 en "01", 2 en "02", etc.
        const siguienteId = siguienteNumero.toString().padStart(2, '0');

        res.json({ siguienteId });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: 'Error al calcular el siguiente ID' });
    }
};

///////////////////////TRAER UN SOCIO POR ID //////////////////////////
// app.get('/api/socios/:id', async (req, res) => {
const getSocioById = async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM socios WHERE idsocio = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Socio no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
};

////////////////////////// GUARDAR UN NUEVO SOCIO //////////////////////////
// app.post('/api/socios', async (req, res) => {
const crearSocio = async (req, res) => {
    const { idsocio, nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente } = req.body;

    try {
        const query = `
            INSERT INTO socios (idsocio, nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
            RETURNING *`;

        // Asegúrate de que idsocio llegue como string y no como [object Object]
        const values = [idsocio, nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente];
        const result = await pool.query(query, values);
        res.status(201).json(result.rows[0]);
    } catch (err) {
        console.error("Error SQL:", err.detail || err.message);
        res.status(500).json({ error: 'Error al guardar: verifique si el ID ya existe' });
    }
};

////////////////////////// ACTUALIZAR UN SOCIO EXISTENTE //////////////////////////
// app.put('/api/socios/:id', async (req, res) => {
const updateSocio = async (req, res) => {
    const { id } = req.params;
    const { nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente } = req.body;

    try {
        const query = `
            UPDATE socios 
            SET nombresocio = $1, 
                aportado = $2, 
                montoprestado = $3, 
                montopagado = $4, 
                montopendiente = $5, 
                interesprestado = $6, 
                interespagado = $7, 
                interesanulado = $8, 
                interespendiente = $9
            WHERE idsocio = $10
            RETURNING *`;

        // AQUÍ ESTABA EL ERROR: Deben ser 6 valores exactamente.
        const values = [
            nombresocio,  // $1
            aportado,     // $2
            montoprestado, // $3
            montopagado,  // $4
            montopendiente, // $5
            interesprestado, // $6
            interespagado,  // $7
            interesanulado, // $8
            interespendiente, // $9
            id            // $10
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Socio no encontrado" });
        }
    } catch (err) {
        console.error("Error al actualizar:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
};

////////////////////////// ELIMINAR UN SOCIO POR ID CON VALIDACIONES DE APORTES Y PRÉSTAMOS //////////////////////////
// app.delete('/api/socios/:id', async (req, res) => {
const eliminarSocio = async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Verificar Aportes
        const aportes = await pool.query('SELECT * FROM aportes WHERE idsocio = $1', [id]);
        if (aportes.rows.length > 0) {
            return res.status(400).json({
                message: 'No se puede eliminar: El socio tiene aportes registrados.'
            });
        }

        // 2. Verificar Préstamos
        const prestamos = await pool.query('SELECT * FROM prestamos WHERE idsocio = $1', [id]);
        if (prestamos.rows.length > 0) {
            return res.status(400).json({
                message: 'No se puede eliminar: El socio tiene préstamos pendientes o activos.'
            });
        }

        // Si pasa las validaciones, procedemos
        await pool.query('DELETE FROM socios WHERE idsocio = $1', [id]);
        res.json({ message: 'Socio eliminado con éxito' });
    } catch (err) {
        res.status(500).json({ error: err.message });
    }
};

module.exports = {
    getSocios,
    getSociosReporte,
    getSiguienteIdSocio,        
    getSocioById,
    crearSocio,
    updateSocio,
    eliminarSocio
};
