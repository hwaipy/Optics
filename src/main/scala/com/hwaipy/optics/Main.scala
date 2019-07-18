package com.hwaipy.optics


object SIM extends App {

  val n = 1.5
  val d = 3
  val RL = 25
  val f = RL / (n - 1)
  val R0 = 1
  val lambda = 1.550e-3
  val k = 2 * math.Pi / lambda
  val phy = lambda / 2 / R0
  val fried_parameter = 100
  val telescope_diameter = 1000
  val mr0 = 400
  val ne0 = mr0
  val pixelSize = 11
  //val x = linspace(-0.5, 0.5, pixelSize)
  //val y = linspace(-0.5, 0.5, pixelSize)
  val R = 3 * R0
  //val r0 = linspace(0, R, mr0)
  //val eta0 = linspace(0, 2 * math.Pi, ne0)
  //  [x0,y0]=pol2cart(theta0,rho0);  %��������ת��Ϊ�ѿ�������
  //val deta = 3 * R0 / (mr0 - 1) * 2 * math.Pi / (ne0 - 1) //积分面积微元
  val zernike_patten_number = 1
  val zernike_level = 6
  //  zernike_phase_cell=cell(1,zernike_level);

  Range(1, zernike_level + 1).foreach(index => {
    val n = math.ceil((-3 + math.sqrt(9 + 8 * index)) / 2).toInt
    val mi = index - (n + 3) * n / 2
    val m = n + mi * 2
    val deltam = if (m == 0) 1 else 0
    val Norm = math.sqrt(2 * (n + 1) / (1 + deltam))
    val Rnm_rho = Range(0, pixelSize).map(i => Range(0, pixelSize).map(_ => 0).toArray).toArray
    Range(0, (n - math.abs(m)) / 2 + 1).foreach(s => {

      //Rnm_rho=Rnm_rho+(-1)^s*prod(1,(n-s))*rho.^(n-2*s)/(prod(1,s)*prod(1,((n+math.abs(m))/2-s))*prod(1,((n-math.abs(m))/2-s)));
    })
    //if m<0
    //Znm=-Norm.*Rnm_rho.*sin(m.*theta);
    //else
    //Znm=Norm.*Rnm_rho.*cos(m.*theta);
    //end
    //
    //nargout
    //
    //if nargout==0
    //surf(x,y,Znm);
    //shading interp;
    //elseif nargout==1
    //varargout{1}=Znm;
    //elseif nargout==2
    //varargout{1}=x;
    //varargout{2}=y;
    //elseif nargout==3
    //varargout{1}=x;
    //varargout{2}=y;
    //varargout{3}=Znm;
    //elseif nargout==4
    //varargout{1}=n;
    //varargout{2}=x;
    //varargout{3}=y;
    //varargout{4}=Znm;
    //end


  })
  //% for index=1:zernike_level
  //%     [zernike_n,x_,y_,z_]=zernikepol_kxyang(index,mr0,mr0,R0);           %��λ����Ӧ��ʼ��ȡ��һ����Χ1
  //  %     zernike_phase_cell(1,index)={z_};
  //%     zernike_index(:,index)=zernike_std(telescope_diameter,fried_parameter,zernike_n)*randn(zernike_patten_number,1);
  //% end


  //val units = x.map(xx => y.map(yy => {
  //  val u = new Unit()
  //  u.x = xx
  //  u.y = yy
  //  u.E1 = math.exp(-(u.x * u.x + u.y * u.y) / R0 / R0)
  //  u
  //}))

  val wavefrontFactory = new WavefrontFactory(telescope_diameter, fried_parameter, 1, 10, 50)
  val capture = new Capture(f, 10, 0.5, 50)
  val tic = System.nanoTime()
  val wavefront = wavefrontFactory.generate
  capture.capture(wavefront)
  val toc = System.nanoTime()

  class Unit {
    val zernike_index = new Array[Double](zernike_level)
    var x = 0.0
    var y = 0.0
    var E1 = 0.0
    var E2 = 0.0

    override def toString: String = s"U($x, $y): E1=$E1"
  }

  println(s"Used ${(toc - tic) / 1e3} us.")
}